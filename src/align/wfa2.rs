use std::ffi::{c_int, CStr};

use wfa2_sys::*;

use super::{Align, AlignAlgorithm, InternalMode};

fn settings(
    algo: &AlignAlgorithm,
    mode: InternalMode,
    pattern_len: usize,
    text_len: usize,
) -> wavefront_aligner_attr_t {
    let mut attributes = unsafe { wavefront_aligner_attr_default };

    attributes.heuristic.strategy = wf_heuristic_strategy_wf_heuristic_none;
    attributes.alignment_scope = alignment_scope_t_compute_alignment;

    attributes.distance_metric = distance_metric_t_gap_affine;
    attributes.affine_penalties.gap_opening = -algo.gap_open as c_int;
    attributes.affine_penalties.gap_extension = -algo.gap_extend as c_int;
    attributes.affine_penalties.mismatch = -algo.mismatch_score as c_int;
    attributes.affine_penalties.match_ = -algo.match_score as c_int;

    match mode {
        InternalMode::Local => {
            attributes.alignment_form.span = alignment_span_t_alignment_endsfree;
            attributes.alignment_form.pattern_begin_free = pattern_len as c_int;
            attributes.alignment_form.pattern_end_free = pattern_len as c_int;
            attributes.alignment_form.text_begin_free = text_len as c_int;
            attributes.alignment_form.text_end_free = text_len as c_int;
            // the ultralow memory mode does not support endsfree alignments
            attributes.memory_mode = wavefront_memory_t_wavefront_memory_high;
        }
        InternalMode::Global => {
            attributes.alignment_form.span = alignment_span_t_alignment_end2end;
            attributes.memory_mode = wavefront_memory_t_wavefront_memory_ultralow;
        }
        InternalMode::Semiglobal => {
            attributes.alignment_form.span = alignment_span_t_alignment_endsfree;
            attributes.alignment_form.pattern_begin_free = 0;
            attributes.alignment_form.pattern_end_free = 0;
            attributes.alignment_form.text_begin_free = text_len as c_int;
            attributes.alignment_form.text_end_free = text_len as c_int;
            attributes.memory_mode = wavefront_memory_t_wavefront_memory_high;
        }
    }

    attributes
}

pub struct Wfa2;

impl Align for Wfa2 {
    fn align(
        &self,
        algo: &AlignAlgorithm,
        mode: InternalMode,
        x: &[u8],
        y: &[u8],
    ) -> Vec<bio::alignment::AlignmentOperation> {
        let mut align_attr = settings(algo, mode, x.len(), y.len());
        let aligner =
            unsafe { wavefront_aligner_new(&mut align_attr as *mut wavefront_aligner_attr_t) };
        unsafe {
            wavefront_align(
                aligner,
                x.as_ptr() as *const i8,
                x.len() as c_int,
                y.as_ptr() as *const i8,
                y.len() as c_int,
            );
        }
        let cigar = unsafe { CStr::from_ptr((*(*aligner).cigar).operations) }
            .to_str()
            .unwrap();
        //eprintln!("{}", cigar);
        let mut ret = vec![];
        for c in cigar.chars() {
            match c {
                'M' => ret.push(bio::alignment::AlignmentOperation::Match),
                'I' => ret.push(bio::alignment::AlignmentOperation::Del),
                'D' => ret.push(bio::alignment::AlignmentOperation::Ins),
                'X' => ret.push(bio::alignment::AlignmentOperation::Subst),
                _ => panic!("Unknown cigar operation: {}", c),
            }
        }
        unsafe { wavefront_aligner_delete(aligner) };
        ret
    }
}
