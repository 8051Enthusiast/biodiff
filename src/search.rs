use std::collections::BTreeSet;
use std::ops::Range;
use std::sync::atomic::Ordering;
use std::{collections::BTreeMap, sync::Arc};

use regex::bytes::{Regex, RegexBuilder};

use crate::file::FileContent;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryType {
    Text,
    Regex,
    Hexagex,
}

#[derive(Clone, Debug)]
pub struct Query {
    text: String,
    query_type: QueryType,
    regex: Arc<Regex>,
}

impl PartialEq for Query {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text && self.query_type == other.query_type
    }
}

impl Eq for Query {}

impl Query {
    pub fn new(query_type: QueryType, text: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let regex = match query_type {
            QueryType::Text => RegexBuilder::new(&regex::escape(text))
                .multi_line(true)
                .unicode(true)
                .build()?,
            QueryType::Regex => RegexBuilder::new(text)
                .multi_line(true)
                .unicode(false)
                .build()?,
            QueryType::Hexagex => hexagex::hexagex(text)?,
        };
        Ok(Query {
            text: text.to_owned(),
            query_type,
            regex: Arc::new(regex),
        })
    }
    pub fn query_type(&self) -> QueryType {
        self.query_type.clone()
    }
    pub fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
/// contains a query and its results
pub struct SearchResults {
    /// maps the start address of a match to its end
    starts: BTreeMap<usize, usize>,
    /// maps the end address of a match to its start
    ends: BTreeMap<usize, usize>,
    /// the query this belongs to
    query: Query,
}

fn map_both<T, S>(r: Result<T, T>, f: impl FnOnce(T) -> S) -> Result<S, S> {
    match r {
        Ok(s) => Ok(f(s)),
        Err(s) => Err(f(s)),
    }
}

fn transpose_both<T>(r: Result<Option<T>, Option<T>>) -> Option<Result<T, T>> {
    match r {
        Ok(Some(s)) => Some(Ok(s)),
        Err(Some(s)) => Some(Err(s)),
        Ok(None) | Err(None) => None,
    }
}

fn unwrap_both<T>(r: Result<T, T>) -> T {
    match r {
        Ok(s) | Err(s) => s,
    }
}

impl SearchResults {
    pub fn new(query: Query) -> Self {
        SearchResults {
            starts: BTreeMap::new(),
            ends: BTreeMap::new(),
            query,
        }
    }
    pub fn query(&self) -> &Query {
        &self.query
    }
    pub fn add_match(&mut self, range: Range<usize>) {
        self.starts.insert(range.start, range.end);
        self.ends.insert(range.end, range.start);
    }
    pub fn lookup_results(&self, range: Range<usize>) -> BTreeSet<(usize, usize)> {
        let mut set = BTreeSet::new();
        set.extend(self.starts.range(range.clone()).map(|x| (*x.0, *x.1)));
        set.extend(self.ends.range(range).map(|x| (*x.1, *x.0)));
        set
    }
    pub fn is_in_result(&self, addr: Option<usize>) -> bool {
        let addr = match addr {
            Some(a) => a,
            None => return false,
        };

        self.starts
            .range(..=addr)
            .rev()
            .next()
            .map_or(false, |(x, y)| (*x..*y).contains(&addr))
    }
    pub fn next_result(&self, addr: usize) -> Option<Result<Range<usize>, Range<usize>>> {
        match self
            .starts
            .range(addr + 1..)
            .map(|(a, b)| *a..*b)
            .next()
            .ok_or_else(|| self.starts.range(..).map(|(a, b)| *a..*b).next())
        {
            Ok(o) => Some(Ok(o)),
            Err(Some(e)) => Some(Err(e)),
            Err(None) => None,
        }
    }
    pub fn nearest_next_result<T: Ord + Copy>(
        list: &[(&Option<Self>, usize, T)],
        to_index: impl Fn(usize, T) -> Option<isize>,
    ) -> Option<isize> {
        let next = list
            .iter()
            .flat_map(|x| x.0.as_ref().into_iter().map(move |y| (y, x.1, x.2)))
            .flat_map(|(search, addr, right)| {
                search
                    .next_result(addr)
                    .and_then(|x| transpose_both(map_both(x, |y| to_index(y.start, right))))
            })
            .min()?;
        Some(unwrap_both(next))
    }
    pub fn prev_result(&self, addr: usize) -> Option<Result<Range<usize>, Range<usize>>> {
        match self
            .ends
            .range(..=addr)
            .rev()
            .map(|(a, b)| *b..*a)
            .next()
            .ok_or_else(|| self.ends.range(..).rev().map(|(a, b)| *b..*a).next())
        {
            Ok(o) => Some(Ok(o)),
            Err(Some(e)) => Some(Err(e)),
            Err(None) => None,
        }
    }
    pub fn nearest_prev_result<T: Ord + Copy>(
        list: &[(&Option<Self>, usize, T)],
        to_index: impl Fn(usize, T) -> Option<isize>,
    ) -> Option<isize> {
        let next = list
            .iter()
            .flat_map(|x| x.0.as_ref().into_iter().map(move |y| (y, x.1, x.2)))
            .flat_map(|(search, addr, right)| {
                search
                    .prev_result(addr)
                    .and_then(|x| transpose_both(map_both(x, |y| to_index(y.start, right))))
                    .map(|x| map_both(x, std::cmp::Reverse))
            })
            .min()?;
        Some(unwrap_both(next).0)
    }
}

#[derive(Clone, Debug)]
pub struct SearchContext {
    pub first: bool,
    pub query: Query,
    pub is_running: Arc<std::sync::atomic::AtomicBool>,
}

impl SearchContext {
    pub fn start_search<Sender>(self, mut send: Sender, file: FileContent)
    where
        Sender: FnMut(Option<Range<usize>>) -> bool + Send + 'static,
    {
        std::thread::spawn(move || {
            for m in self.query.regex.find_iter(&file) {
                let r = if self.is_running.load(Ordering::Relaxed) {
                    Some(m.range())
                } else {
                    None
                };
                let res = send(r.clone());
                if !res || r.is_none() {
                    return;
                }
            }
            send(None);
        });
    }
}
