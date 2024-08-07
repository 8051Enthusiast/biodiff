use serde::{Deserialize, Serialize};

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

/// The RustBio aligner, with optional banding.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct RustBio {
    pub band: Banded,
}

#[cfg(feature = "bio")]
/// Whether the RustBio backend is available.
pub const RUSTBIO_AVAILABLE: bool = true;
#[cfg(not(feature = "bio"))]
/// Whether the RustBio backend is available.
pub const RUSTBIO_AVAILABLE: bool = false;

#[cfg(feature = "bio")]
mod implemented {
    use crate::{Align, AlignAlgorithm, CheckStatus, InternalMode, Op};
    use bio::alignment::{
        pairwise::{self, MatchFunc, Scoring},
        AlignmentOperation,
    };

    use super::*;
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

    impl Align for RustBio {
        fn align(&self, algo: &AlignAlgorithm, mode: InternalMode, x: &[u8], y: &[u8]) -> Vec<Op> {
            let scoring = scorer(algo, mode);
            let res = match self.band {
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
            };
            res.into_iter()
                .map(|op| match op {
                    AlignmentOperation::Match => Op::Match,
                    AlignmentOperation::Subst => Op::Subst,
                    AlignmentOperation::Del => Op::Del,
                    AlignmentOperation::Ins => Op::Ins,
                    AlignmentOperation::Xclip(x) => Op::Xclip(x),
                    AlignmentOperation::Yclip(y) => Op::Yclip(y),
                })
                .collect()
        }

        fn check_params(
            &self,
            _: &AlignAlgorithm,
            _: InternalMode,
            x_size: usize,
            y_size: usize,
        ) -> CheckStatus {
            const SIZE_LIMIT: u64 = 1 << 30;
            if x_size as u64 * y_size as u64 > SIZE_LIMIT {
                return CheckStatus::MemoryWarning;
            }
            CheckStatus::Ok
        }
    }
}

#[cfg(not(feature = "bio"))]
mod unimplemented {
    use crate::{Align, CheckStatus, Op};

    impl Align for super::RustBio {
        fn align(
            &self,
            _algo: &crate::AlignAlgorithm,
            _mode: crate::InternalMode,
            _x: &[u8],
            _y: &[u8],
        ) -> Vec<Op> {
            unimplemented!()
        }

        fn check_params(
            &self,
            _algo: &crate::AlignAlgorithm,
            _mode: crate::InternalMode,
            _x_size: usize,
            _y_size: usize,
        ) -> CheckStatus {
            return CheckStatus::Error(String::from(
                "Rust-bio is not available. Please recompile with the\n\
                `bio` feature enabled or choose another algorithm.",
            ));
        }
    }
}
