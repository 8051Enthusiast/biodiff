use serde::{Deserialize, Serialize};

use crate::align::{AlgorithmKind, AlignAlgorithm, AlignInfo, AlignMode};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PresetCursor {
    pub preset: Option<u32>,
    pub kind: AlgorithmKind,
}

/// We maintain two separate lists of alignment algorithm presets,
/// one for global alignment when pressing f3 without selection,
/// and one for semiglobal when pressing f3 with one side selected.
/// This is mostly because not every backend supports semiglobal alignment.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PresetList {
    pub global: Vec<AlignAlgorithm>,
    // don't use usize here as this field is serialized and we want
    // to use it on different architectures
    pub current_global: u32,
    pub semiglobal: Vec<AlignAlgorithm>,
    pub current_semiglobal: u32,
}

impl Default for PresetList {
    fn default() -> Self {
        let blockwise = AlignAlgorithm::default();
        let mut global = blockwise.clone();
        global.name = "global".to_string();
        global.mode = AlignMode::Global;
        let default = AlignAlgorithm::default_semiglobal();
        PresetList {
            global: vec![blockwise, global],
            current_global: 0,
            semiglobal: vec![default],
            current_semiglobal: 0,
        }
    }
}

impl PresetList {
    pub fn current_global(&self) -> &AlignAlgorithm {
        &self.global[self.current_global as usize]
    }
    pub fn current_semiglobal(&self) -> &AlignAlgorithm {
        &self.semiglobal[self.current_semiglobal as usize]
    }
    pub fn current_info(&self) -> AlignInfo {
        AlignInfo {
            global: self.current_global().clone(),
            semiglobal: self.current_semiglobal().clone(),
        }
    }
    /// gets the current preset at the cursor, giving back the default
    /// if the cursor doesn't have an index
    pub fn get(&self, cursor: PresetCursor) -> AlignAlgorithm {
        match cursor {
            PresetCursor {
                preset: Some(preset),
                kind: AlgorithmKind::Global,
            } => self.global[preset as usize].clone(),
            PresetCursor {
                preset: Some(preset),
                kind: AlgorithmKind::Semiglobal,
            } => self.semiglobal[preset as usize].clone(),
            PresetCursor {
                preset: None,
                kind: AlgorithmKind::Global,
            } => AlignAlgorithm::default(),
            PresetCursor {
                preset: None,
                kind: AlgorithmKind::Semiglobal,
            } => AlignAlgorithm::default_semiglobal(),
        }
    }
    /// deletes the preset at `cursor`, returning false if deleting
    /// would leave 0 items
    pub fn delete(&mut self, cursor: PresetCursor) -> bool {
        let arr = match cursor.kind {
            AlgorithmKind::Global => &mut self.global,
            AlgorithmKind::Semiglobal => &mut self.semiglobal,
        };
        let current = match cursor.kind {
            AlgorithmKind::Global => &mut self.current_global,
            AlgorithmKind::Semiglobal => &mut self.current_semiglobal,
        };
        if arr.len() <= 1 {
            return false;
        }
        let index = cursor.preset.unwrap_or(*current);
        arr.remove(index as usize);
        if *current > index {
            *current -= 1;
        }
        *current = (arr.len() as u32 - 1).min(*current);
        true
    }
    /// finds the preset with the given name, returning a cursor
    pub fn find(&self, kind: AlgorithmKind, name: &str) -> PresetCursor {
        let arr = match kind {
            AlgorithmKind::Global => &self.global,
            AlgorithmKind::Semiglobal => &self.semiglobal,
        };
        let preset = arr.iter().position(|preset| preset.name == name);
        PresetCursor {
            preset: preset.map(|x| x as u32),
            kind,
        }
    }
    /// selects the preset at the cursor
    pub fn select(&mut self, cursor: PresetCursor) {
        let current = match cursor.kind {
            AlgorithmKind::Global => &mut self.current_global,
            AlgorithmKind::Semiglobal => &mut self.current_semiglobal,
        };
        let len = match cursor.kind {
            AlgorithmKind::Global => self.global.len(),
            AlgorithmKind::Semiglobal => self.semiglobal.len(),
        };
        if let Some(preset) = cursor.preset {
            if preset < len as u32 {
                *current = preset;
            }
        }
    }
    /// sets the preset at the cursor, returning false if the name is
    /// already taken
    pub fn set(&mut self, cursor: PresetCursor, settings: AlignAlgorithm) -> bool {
        let arr = match cursor.kind {
            AlgorithmKind::Global => &mut self.global,
            AlgorithmKind::Semiglobal => &mut self.semiglobal,
        };

        // we should not allow two presets with the same name for the same kind
        let has_name_already = arr
            .iter()
            .enumerate()
            .any(|(i, preset)| Some(i as u32) != cursor.preset && preset.name == settings.name);
        if has_name_already {
            return false;
        }

        match cursor.preset {
            Some(preset) => arr[preset as usize] = settings,
            None => arr.push(settings),
        }
        true
    }
}
