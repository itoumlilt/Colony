mod lru_cache;
mod supported_term;

// std
use std::sync::Mutex;

use rustler::resource::ResourceArc;
use rustler::{Atom, Env, Term};

use crate::lru_cache::LruCache;
use crate::supported_term::SupportedTerm;

mod atoms {
    rustler::atoms! {
        // Common Atoms
        ok,
        error,

        // Resource Atoms
        // bad_reference,
        lock_fail,

        // Error Atoms
        empty,
        not_found,
        unsupported_type,
    }
}

pub struct LruCacheResource(Mutex<LruCache>);

type LruCacheArc = ResourceArc<LruCacheResource>;

fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(LruCacheResource, env);
    true
}


#[rustler::nif]
fn new(capacity: usize) -> (Atom, LruCacheArc) {
    let resource = ResourceArc::new(
        LruCacheResource(Mutex::new(LruCache::new(capacity))));

    (atoms::ok(), resource)
}

#[rustler::nif]
fn from_keys(capacity: usize, key_vec: Vec<Term>, value: Term) -> Result<LruCacheArc, Atom> {
    let value = match SupportedTerm::from(&value) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let mut lru_cache = LruCache::new(capacity);

    for key in key_vec {
        let key = match SupportedTerm::from(&key) {
            Some(term) => term,
            _ => return Err(atoms::unsupported_type())
        };
        lru_cache.put(key, value.clone());
    }

    Ok(ResourceArc::new(LruCacheResource(Mutex::new(lru_cache))))
}


#[rustler::nif]
fn from_list(capacity: usize, kv_list: Vec<(Term, Term)>) -> Result<LruCacheArc, Atom> {
    let mut lru_cache = LruCache::new(capacity);

    for (key, value) in kv_list {
        let key = match SupportedTerm::from(&key) {
            Some(term) => term,
            _ => return Err(atoms::unsupported_type())
        };
        let value = match SupportedTerm::from(&value) {
            Some(term) => term,
            _ => return Err(atoms::unsupported_type())
        };
        lru_cache.put(key, value);
    }

    Ok(ResourceArc::new(LruCacheResource(Mutex::new(lru_cache))))
}

#[rustler::nif]
fn put(resource: ResourceArc<LruCacheResource>, k: Term, v: Term)
       -> Result<Option<(SupportedTerm, SupportedTerm)>, Atom> {
    let k = match SupportedTerm::from(&k) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };
    let v = match SupportedTerm::from(&v) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    let removed = lru_cache.put(k, v);
    return Ok(removed);
}

#[rustler::nif]
fn get(resource: ResourceArc<LruCacheResource>, k: Term)
       -> Result<SupportedTerm, Atom> {
    let k = match SupportedTerm::from(&k) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match lru_cache.get(&k) {
        None => Err(atoms::not_found()),
        Some(v) => Ok(v.clone())
    }
}

#[rustler::nif]
fn peek(resource: ResourceArc<LruCacheResource>, k: Term)
       -> Result<SupportedTerm, Atom> {
    let k = match SupportedTerm::from(&k) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match lru_cache.peek(&k) {
        None => Err(atoms::not_found()),
        Some(v) => Ok(v.clone())
    }
}

#[rustler::nif]
fn peek_lru(resource: ResourceArc<LruCacheResource>)
        -> Result<(SupportedTerm, SupportedTerm), Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match lru_cache.peek_lru() {
        None => Err(atoms::empty()),
        Some((k, v)) => Ok((k.clone(), v.clone()))
    }
}

#[rustler::nif]
fn contains(resource: ResourceArc<LruCacheResource>, k: Term)
        -> Result<bool, Atom> {
    let k = match SupportedTerm::from(&k) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.contains(&k))
}


#[rustler::nif]
fn pop(resource: ResourceArc<LruCacheResource>, k: Term)
            -> Result<SupportedTerm, Atom> {
    let k = match SupportedTerm::from(&k) {
        Some(term) => term,
        _ => return Err(atoms::unsupported_type())
    };

    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match lru_cache.pop(&k) {
        None => Err(atoms::not_found()),
        Some(v) => Ok(v.clone())
    }
}


#[rustler::nif]
fn pop_lru(resource: ResourceArc<LruCacheResource>)
       -> Result<(SupportedTerm, SupportedTerm), Atom> {
    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match lru_cache.pop_lru() {
        None => Err(atoms::empty()),
        Some(kv) => Ok(kv)
    }
}


#[rustler::nif]
fn len(resource: ResourceArc<LruCacheResource>)
            -> Result<usize, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.len())
}

#[rustler::nif]
fn is_empty(resource: ResourceArc<LruCacheResource>)
       -> Result<bool, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.is_empty())
}

#[rustler::nif]
fn cap(resource: ResourceArc<LruCacheResource>)
       -> Result<usize, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.cap())
}

#[rustler::nif]
fn resize(resource: ResourceArc<LruCacheResource>, capacity: usize)
          -> Result<Vec<(SupportedTerm, SupportedTerm)>, Atom> {
    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.resize(capacity))
}

#[rustler::nif]
fn clear(resource: ResourceArc<LruCacheResource>)
       -> Atom {
    let mut lru_cache = match resource.0.try_lock() {
        Err(_) => return atoms::lock_fail(),
        Ok(guard) => guard,
    };

    lru_cache.clear();
    atoms::ok()
}

#[rustler::nif]
fn keys(resource: ResourceArc<LruCacheResource>)
         -> Result<Vec<SupportedTerm>, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.keys())
}

#[rustler::nif]
fn values(resource: ResourceArc<LruCacheResource>)
        -> Result<Vec<SupportedTerm>, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.values())
}

#[rustler::nif]
fn to_list(resource: ResourceArc<LruCacheResource>)
        -> Result<Vec<(SupportedTerm, SupportedTerm)>, Atom> {
    let lru_cache = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(lru_cache.to_vec())
}

rustler::init!("Elixir.LruCacheNif",[
    new, from_keys, from_list,
    put, get, peek, peek_lru, contains, pop, pop_lru,
    len, is_empty, cap, resize, clear, keys, values, to_list], load = load);
