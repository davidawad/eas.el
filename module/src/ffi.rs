// SPDX-License-Identifier: GPL-3.0-or-later
//! A minimal hand-written binding of emacs-module.h.
//!
//! Only the fields of `struct emacs_env_25' are declared: every later
//! environment (26 to 30) starts with them in this order, so the layout
//! is stable across Emacs versions.  The module checks the size Emacs
//! reports before using it.

use std::ffi::CString;
use std::os::raw::{c_char, c_int, c_void};

#[repr(C)]
pub struct ValueTag {
    _private: [u8; 0],
}
pub type Value = *mut ValueTag;

pub type Function =
    unsafe extern "C" fn(env: *mut Env, nargs: isize, args: *mut Value, data: *mut c_void) -> Value;
pub type Finalizer = unsafe extern "C" fn(data: *mut c_void);

#[repr(C)]
pub struct Runtime {
    pub size: isize,
    pub private_members: *mut c_void,
    pub get_environment: unsafe extern "C" fn(runtime: *mut Runtime) -> *mut Env,
}

/// `struct emacs_env_25'.
#[repr(C)]
pub struct Env {
    pub size: isize,
    pub private_members: *mut c_void,
    pub make_global_ref: unsafe extern "C" fn(*mut Env, Value) -> Value,
    pub free_global_ref: unsafe extern "C" fn(*mut Env, Value),
    pub non_local_exit_check: unsafe extern "C" fn(*mut Env) -> c_int,
    pub non_local_exit_clear: unsafe extern "C" fn(*mut Env),
    pub non_local_exit_get: unsafe extern "C" fn(*mut Env, *mut Value, *mut Value) -> c_int,
    pub non_local_exit_signal: unsafe extern "C" fn(*mut Env, Value, Value),
    pub non_local_exit_throw: unsafe extern "C" fn(*mut Env, Value, Value),
    pub make_function:
        unsafe extern "C" fn(*mut Env, isize, isize, Function, *const c_char, *mut c_void) -> Value,
    pub funcall: unsafe extern "C" fn(*mut Env, Value, isize, *mut Value) -> Value,
    pub intern: unsafe extern "C" fn(*mut Env, *const c_char) -> Value,
    pub type_of: unsafe extern "C" fn(*mut Env, Value) -> Value,
    pub is_not_nil: unsafe extern "C" fn(*mut Env, Value) -> bool,
    pub eq: unsafe extern "C" fn(*mut Env, Value, Value) -> bool,
    pub extract_integer: unsafe extern "C" fn(*mut Env, Value) -> i64,
    pub make_integer: unsafe extern "C" fn(*mut Env, i64) -> Value,
    pub extract_float: unsafe extern "C" fn(*mut Env, Value) -> f64,
    pub make_float: unsafe extern "C" fn(*mut Env, f64) -> Value,
    pub copy_string_contents: unsafe extern "C" fn(*mut Env, Value, *mut c_char, *mut isize) -> bool,
    pub make_string: unsafe extern "C" fn(*mut Env, *const c_char, isize) -> Value,
    pub make_user_ptr: unsafe extern "C" fn(*mut Env, Option<Finalizer>, *mut c_void) -> Value,
    pub get_user_ptr: unsafe extern "C" fn(*mut Env, Value) -> *mut c_void,
    pub set_user_ptr: unsafe extern "C" fn(*mut Env, Value, *mut c_void),
    pub get_user_finalizer: unsafe extern "C" fn(*mut Env, Value) -> Option<Finalizer>,
    pub set_user_finalizer: unsafe extern "C" fn(*mut Env, Value, Option<Finalizer>),
    pub vec_get: unsafe extern "C" fn(*mut Env, Value, isize) -> Value,
    pub vec_set: unsafe extern "C" fn(*mut Env, Value, isize, Value),
    pub vec_size: unsafe extern "C" fn(*mut Env, Value) -> isize,
}

/// An error to signal back to Emacs.
pub struct Error(pub String);

pub type Result<T> = std::result::Result<T, Error>;

/// A borrowed environment for the duration of one call from Emacs.
#[derive(Clone, Copy)]
pub struct E(pub *mut Env);

impl E {
    #[inline]
    fn env(&self) -> &Env {
        // SAFETY: Emacs passes a valid environment for the call.
        unsafe { &*self.0 }
    }

    /// Fail when a non-local exit is pending (and leave it pending).
    pub fn check(&self) -> Result<()> {
        // SAFETY: valid environment.
        if unsafe { (self.env().non_local_exit_check)(self.0) } != 0 {
            Err(Error(String::new()))
        } else {
            Ok(())
        }
    }

    fn clear(&self) {
        // SAFETY: valid environment.
        unsafe { (self.env().non_local_exit_clear)(self.0) }
    }

    pub fn intern(&self, name: &str) -> Value {
        let c = CString::new(name).unwrap();
        // SAFETY: valid environment and NUL-terminated name.
        unsafe { (self.env().intern)(self.0, c.as_ptr()) }
    }

    pub fn is_not_nil(&self, v: Value) -> bool {
        // SAFETY: valid environment.
        unsafe { (self.env().is_not_nil)(self.0, v) }
    }

    /// A number, integer or float, as a float; an error for anything else.
    #[inline]
    pub fn num(&self, v: Value) -> Result<f64> {
        // SAFETY: valid environment.  extract_float returns 0.0 on a
        // signal, so only then is the exit checked: a float 0.0 passes.
        let f = unsafe { (self.env().extract_float)(self.0, v) };
        if f != 0.0 || f.is_nan() {
            return Ok(f);
        }
        if self.check().is_ok() {
            return Ok(f);
        }
        self.clear();
        // SAFETY: valid environment.
        let i = unsafe { (self.env().extract_integer)(self.0, v) };
        self.check().map_err(|_| Error("number expected".into()))?;
        Ok(i as f64)
    }

    pub fn int(&self, v: Value) -> Result<i64> {
        // SAFETY: valid environment.
        let i = unsafe { (self.env().extract_integer)(self.0, v) };
        self.check().map(|_| i)
    }

    #[inline]
    pub fn float(&self, f: f64) -> Value {
        // SAFETY: valid environment.
        unsafe { (self.env().make_float)(self.0, f) }
    }

    pub fn integer(&self, i: i64) -> Value {
        // SAFETY: valid environment.
        unsafe { (self.env().make_integer)(self.0, i) }
    }

    pub fn string(&self, s: &str) -> Value {
        // SAFETY: valid environment; S is UTF-8 of the given length.
        unsafe { (self.env().make_string)(self.0, s.as_ptr() as *const c_char, s.len() as isize) }
    }

    pub fn get_string(&self, v: Value) -> Result<String> {
        let mut len: isize = 0;
        // SAFETY: valid environment; a null buffer asks for the size.
        unsafe { (self.env().copy_string_contents)(self.0, v, std::ptr::null_mut(), &mut len) };
        self.check()?;
        let mut buf = vec![0u8; len.max(1) as usize];
        // SAFETY: BUF holds LEN bytes.
        unsafe { (self.env().copy_string_contents)(self.0, v, buf.as_mut_ptr() as *mut c_char, &mut len) };
        self.check()?;
        buf.truncate((len.max(1) - 1) as usize);
        String::from_utf8(buf).map_err(|_| Error("bad string".into()))
    }

    #[inline]
    pub fn vec_get(&self, v: Value, i: usize) -> Value {
        // SAFETY: valid environment; a bad index signals.
        unsafe { (self.env().vec_get)(self.0, v, i as isize) }
    }

    #[inline]
    pub fn vec_size(&self, v: Value) -> Result<usize> {
        // SAFETY: valid environment; a non-vector signals.
        let n = unsafe { (self.env().vec_size)(self.0, v) };
        self.check().map(|_| n as usize)
    }

    pub fn funcall(&self, f: Value, args: &mut [Value]) -> Value {
        // SAFETY: valid environment; ARGS outlives the call.
        unsafe { (self.env().funcall)(self.0, f, args.len() as isize, args.as_mut_ptr()) }
    }

    pub fn user_ptr(&self, fin: Finalizer, p: *mut c_void) -> Value {
        // SAFETY: valid environment.
        unsafe { (self.env().make_user_ptr)(self.0, Some(fin), p) }
    }

    pub fn get_user_ptr(&self, v: Value) -> Result<*mut c_void> {
        // SAFETY: valid environment; a non-user-ptr signals.
        let p = unsafe { (self.env().get_user_ptr)(self.0, v) };
        self.check()?;
        Ok(p)
    }

    pub fn user_finalizer(&self, v: Value) -> Option<Finalizer> {
        // SAFETY: valid environment.
        let f = unsafe { (self.env().get_user_finalizer)(self.0, v) };
        if self.check().is_err() {
            self.clear();
            return None;
        }
        f
    }

    /// Signal (error MESSAGE) unless an exit is already pending.
    pub fn signal(&self, msg: &str) {
        if self.check().is_err() {
            return;
        }
        let err = self.intern("error");
        let s = self.string(msg);
        let list = self.intern("list");
        let data = self.funcall(list, &mut [s]);
        // SAFETY: valid environment.
        unsafe { (self.env().non_local_exit_signal)(self.0, err, data) }
    }

    /// Define NAME as FUNC taking MIN to MAX arguments.
    pub fn defun(&self, name: &str, min: isize, max: isize, func: Function, doc: &str) {
        let d = CString::new(doc).unwrap();
        // SAFETY: valid environment; the docstring is copied by Emacs.
        let f = unsafe { (self.env().make_function)(self.0, min, max, func, d.as_ptr(), std::ptr::null_mut()) };
        let defalias = self.intern("defalias");
        let sym = self.intern(name);
        self.funcall(defalias, &mut [sym, f]);
    }
}
