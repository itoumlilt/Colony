use crate::supported_term::SupportedTerm;

pub struct LruCache {
    cache: lru::LruCache<SupportedTerm, SupportedTerm>,
}

impl LruCache {
    pub fn new(cap: usize) -> LruCache {
        LruCache {
            cache: lru::LruCache::new(cap)
        }
    }

    /// 返回因超过容量而被移除的key-value，如果每超过容量则返回None
    pub fn put(&mut self, k: SupportedTerm, v: SupportedTerm)
               -> Option<(SupportedTerm, SupportedTerm)> {
        if self.cache.len() < self.cache.cap() || self.cache.contains(&k) {
            self.cache.put(k, v);
            return None;
        }

        let removed = self.cache.pop_lru();
        self.cache.put(k, v);
        return removed;
    }

    pub fn get(&mut self, k: &SupportedTerm) -> Option<&SupportedTerm> {
        self.cache.get(k)
    }

    pub fn peek(&self, k: &SupportedTerm) -> Option<&SupportedTerm> {
        self.cache.peek(k)
    }

    pub fn peek_lru<'a>(&self) -> Option<(&'a SupportedTerm, &'a SupportedTerm)> {
        self.cache.peek_lru()
    }

    pub fn contains(&self, k: &SupportedTerm) -> bool {
        self.cache.contains(k)
    }

    pub fn pop(&mut self, k: &SupportedTerm) -> Option<SupportedTerm> {
        self.cache.pop(k)
    }

    pub fn pop_lru(&mut self) -> Option<(SupportedTerm, SupportedTerm)> {
        self.cache.pop_lru()
    }

    pub fn len(&self) -> usize {
        self.cache.len()
    }

    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    pub fn cap(&self) -> usize {
        self.cache.cap()
    }

    /// 返回因容量变小而移除的key-value列表，如果容量变大则返回空列表
    pub fn resize(&mut self, cap: usize) -> Vec<(SupportedTerm, SupportedTerm)> {
        let len = self.cache.len();
        if len <= cap {
            self.cache.resize(cap);
            return Vec::new();
        }

        let mut count = len - cap;
        let mut vec = Vec::with_capacity(count);
        while count > 0 {
            match self.cache.pop_lru() {
                None => break,
                Some(removed) => vec.push(removed)
            }
            count = count - 1;
        }
        self.cache.resize(cap);
        return vec;
    }

    pub fn clear(&mut self) {
        self.cache.clear()
    }

    pub fn keys(&self) -> Vec<SupportedTerm> {
        let mut key_vec = Vec::with_capacity(self.cache.len());
        for (k, _v) in self.cache.iter() {
            key_vec.push(k.clone());
        }
        key_vec
    }

    pub fn values(&self) -> Vec<SupportedTerm> {
        let mut value_vec = Vec::with_capacity(self.cache.len());
        for (_k, v) in self.cache.iter() {
            value_vec.push(v.clone());
        }
        value_vec
    }

    pub fn to_vec(&self) -> Vec<(SupportedTerm, SupportedTerm)> {
        let mut new_vec = Vec::with_capacity(self.cache.len());
        for (k, v) in self.cache.iter() {
            new_vec.push((k.clone(), v.clone()))
        }
        new_vec
    }
}
//
// impl Drop for LruCache {
//     fn drop(&mut self) {
//         write_file("LruCache.drop.txt", format!("LruCache::drop - {:?}", timestamp1()));
//     }
// }
//
// fn write_file(path: &str, data: String) {
//     let mut f = std::fs::File::create(path).expect("Unable to create file");
//     f.write_all(data.as_bytes()).expect("Unable to write data");
// }
//
// fn timestamp1() -> i64 {
//     let start = std::time::SystemTime::now();
//     let since_the_epoch = start
//         .duration_since(std::time::UNIX_EPOCH)
//         .expect("Time went backwards");
//     let ms = since_the_epoch.as_secs() as i64 * 1000i64 + (since_the_epoch.subsec_nanos() as f64 / 1_000_000.0) as i64;
//     ms
// }