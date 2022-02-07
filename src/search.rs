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
                    return
                }
            }
            send(None);
        });
    }
}
