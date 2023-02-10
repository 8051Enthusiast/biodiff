use bio::alignment::AlignmentOperation;
use block_aligner::{
    cigar::Cigar,
    scan_block::{Block, PaddedBytes},
    scores::{ByteMatrix, Gaps},
};

use super::Align;

pub struct BlockAligner;

impl Align for BlockAligner {
    fn align(
        &self,
        algo: &super::AlignAlgorithm,
        mode: super::InternalMode,
        x: &[u8],
        y: &[u8],
    ) -> Vec<AlignmentOperation> {
        if !matches!(mode, super::InternalMode::Global) {
            panic!("BlockAligner only supports global alignment");
        }
        let gaps = Gaps {
            open: algo.gap_open as i8,
            extend: algo.gap_extend as i8,
        };
        let max_block_size = (x.len().min(y.len()) / 4).next_power_of_two().max(16);
        eprintln!("max_block_size: {}", max_block_size);
        let min_block_size = (max_block_size / 8).max(16);
        eprintln!("min_block_size: {}", min_block_size);
        let matrix = ByteMatrix::new_simple(algo.match_score as i8, algo.mismatch_score as i8);
        let x_padded = PaddedBytes::from_bytes::<ByteMatrix>(&x, max_block_size);
        let y_padded = PaddedBytes::from_bytes::<ByteMatrix>(&y, max_block_size);
        let mut a = Block::<true, false>::new(x.len(), y.len(), max_block_size);
        a.align(
            &x_padded,
            &y_padded,
            &matrix,
            gaps,
            min_block_size..=max_block_size,
            0,
        );
        let mut cigar = Cigar::new(x.len(), y.len());
        a.trace().cigar(x.len(), y.len(), &mut cigar);
        let mut ret = Vec::new();
        let mut xaddr = 0;
        let mut yaddr = 0;
        for el in cigar.to_vec() {
            for _ in 0..el.len {
                match el.op {
                    block_aligner::cigar::Operation::M => {
                        if x.get(xaddr) == y.get(yaddr) {
                            ret.push(AlignmentOperation::Match);
                        } else {
                            ret.push(AlignmentOperation::Subst);
                        }
                        xaddr += 1;
                        yaddr += 1;
                    }
                    block_aligner::cigar::Operation::I => {
                        ret.push(AlignmentOperation::Ins);
                        yaddr += 1;
                    }
                    block_aligner::cigar::Operation::D => {
                        ret.push(AlignmentOperation::Del);
                        xaddr += 1;
                    }
                    block_aligner::cigar::Operation::Sentinel => unreachable!(),
                }
            }
        }
        ret
    }
}
