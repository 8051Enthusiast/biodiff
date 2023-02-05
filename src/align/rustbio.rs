use bio::alignment::{
    pairwise::{self, MatchFunc, Scoring},
    AlignmentOperation,
};

use super::{Align, AlignAlgorithm, InternalMode};

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
        InternalMode::Local => scorer.xclip(0).yclip(0),
        InternalMode::Global => scorer,
        InternalMode::Semiglobal => scorer.yclip(0),
    }
}

pub struct RustBio;

impl Align for RustBio {
    fn align(
        &self,
        algo: &AlignAlgorithm,
        mode: InternalMode,
        x: &[u8],
        y: &[u8],
    ) -> Vec<AlignmentOperation> {
        let scoring = scorer(algo, mode);
        pairwise::Aligner::with_scoring(scoring)
            .custom(x, y)
            .operations
    }
}

pub fn align_banded(
    algo: &AlignAlgorithm,
    mode: InternalMode,
    x: &[u8],
    y: &[u8],
) -> Vec<AlignmentOperation> {
    let (kmer, window) = match algo.band {
        super::Banded::Normal => return RustBio.align(algo, mode, x, y),
        super::Banded::Banded { kmer, window } => (kmer, window),
    };
    let scoring = scorer(algo, mode);
    pairwise::banded::Aligner::with_scoring(scoring, kmer, window)
        .custom(x, y)
        .operations
}
