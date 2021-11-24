use crate::parser::StrId;
use crate::value::Value;
use crate::FnvIndexMap;

pub type EnvId = u32;

pub struct EnvStore {
    count: u32,
    pub current: EnvId,
    pub capture: bool,
    parents: FnvIndexMap<EnvId, EnvId>,
    values: FnvIndexMap<(EnvId, StrId), Value>,
}

impl EnvStore {
    pub fn new() -> Self {
        Self {
            count: 0,
            current: 0,
            capture: false,
            parents: Default::default(),
            values: Default::default(),
        }
    }

    pub fn create(&mut self, parent: EnvId) -> EnvId {
        self.count += 1;

        if parent > 0 {
            self.parents.insert(self.count, parent);
        }

        self.count
    }

    pub fn parent(&self, child: EnvId) -> EnvId {
        *self.parents.get(&child).unwrap_or(&0)
    }

    pub fn extend(&mut self, env: EnvId, id: StrId, val: Value) -> bool {
        self.values.insert((env, id), val).is_none()
    }

    pub fn pop(&mut self, env: EnvId) {
        if !self.capture && env != self.current {
            while let Some((key, _)) = self.values.last() {
                if key.0 == env {
                    self.values.pop().unwrap();
                } else {
                    break;
                }
            }

            self.parents.remove(&env);
        }
    }

    pub fn get(&mut self, id: StrId) -> Option<Value> {
        let mut env = self.current;

        loop {
            match self.values.get(&(env, id)) {
                None if env > 0 => env = self.parent(env),
                opt => return opt.copied(),
            }
        }
    }

    pub fn set(&mut self, id: StrId, val: Value) -> bool {
        let mut env = self.current;

        loop {
            if let Some(refer) = self.values.get_mut(&(env, id)) {
                *refer = val;
                return true;
            } else if env > 0 {
                env = self.parent(env);
            } else {
                return false;
            }
        }
    }
}
