use bio::alignment::{
    pairwise::{self, MatchFunc, Scoring},
    AlignmentOperation,
};
use serde::{Deserialize, Serialize};

use super::{Align, AlignAlgorithm, InternalMode};

pub const DEFAULT_KMER: usize = 8;
pub const DEFAULT_WINDOW: usize = 6;
/// Determines whether to use the banded variant of the algorithm with given k-mer length
/// and window size for the rustbio backend
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Default)]
pub enum Banded {
    #[default]
    Normal,
    Banded {
        kmer: usize,
        window: usize,
    },
}
fn scorer(
    algo: &AlignAlgorithm,
    mode: InternalMode,
) -> Scoring<impl MatchFunc + Copy + Send + Sync> {
    let match_score = algo.match_score;
    let mismatch_score = algo.mismatch_score;
    let score = move |a: u8, b: u8| {
        if a == b {
            match_score
        } else {
            mismatch_score
        }
    };
    let scorer = Scoring::new(algo.gap_open, algo.gap_extend, score);
    match mode {
        InternalMode::Global => scorer,
        InternalMode::Semiglobal => scorer.yclip(0),
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct RustBio {
    pub band: Banded,
}

impl Align for RustBio {
    fn align(
        &self,
        algo: &AlignAlgorithm,
        mode: InternalMode,
        x: &[u8],
        y: &[u8],
    ) -> Vec<AlignmentOperation> {
        let scoring = scorer(algo, mode);
        match self.band {
            Banded::Normal => {
                pairwise::Aligner::with_scoring(scoring)
                    .custom(x, y)
                    .operations
            }
            Banded::Banded { kmer, window } => {
                pairwise::banded::Aligner::with_scoring(scoring, kmer, window)
                    .custom(x, y)
                    .operations
            }
        }
    }
}
