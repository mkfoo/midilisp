use crate::value::Value;
use fnv::FnvBuildHasher;
use indexmap::IndexMap;

type FnvIndexMap<K, V> = IndexMap<K, V, FnvBuildHasher>;

pub struct EnvStore {
    count: u32,
    pub current: u32,
    pub capture: bool,
    parents: FnvIndexMap<u32, u32>,
    values: FnvIndexMap<(u32, u32), Value>,
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

    pub fn create(&mut self, parent: u32) -> u32 {
        self.count += 1;

        if parent > 0 {
            self.parents.insert(self.count, parent);
        }

        self.count
    }

    pub fn parent(&self, child: u32) -> u32 {
        *self.parents.get(&child).unwrap_or(&0)
    }

    pub fn extend(&mut self, env: u32, id: u32, val: Value) -> bool {
        self.values.insert((env, id), val).is_none()
    }

    pub fn pop(&mut self, env: u32) {
        if !self.capture {
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

    pub fn get(&mut self, id: u32) -> Option<Value> {
        let mut env = self.current;

        loop {
            match self.values.get(&(env, id)).copied() {
                None if env > 0 => env = self.parent(env),
                opt => return opt,
            }
        }
    }

    pub fn set(&mut self, id: u32, val: Value) -> bool {
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
