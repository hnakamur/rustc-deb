use std::marker;
use std::mem;
use std::ptr;

use {raw, Oid, ObjectType, Error, Buf, Commit, Tag, Blob, Tree, Repository};
use {Describe, DescribeOptions};
use util::Binding;

/// A structure to represent a git [object][1]
///
/// [1]: http://git-scm.com/book/en/Git-Internals-Git-Objects
pub struct Object<'repo> {
    raw: *mut raw::git_object,
    _marker: marker::PhantomData<&'repo Repository>,
}

impl<'repo> Object<'repo> {
    /// Get the id (SHA1) of a repository object
    pub fn id(&self) -> Oid {
        unsafe {
            Binding::from_raw(raw::git_object_id(&*self.raw))
        }
    }

    /// Get the object type of an object.
    ///
    /// If the type is unknown, then `None` is returned.
    pub fn kind(&self) -> Option<ObjectType> {
        ObjectType::from_raw(unsafe { raw::git_object_type(&*self.raw) })
    }

    /// Recursively peel an object until an object of the specified type is met.
    ///
    /// If you pass `Any` as the target type, then the object will be
    /// peeled until the type changes (e.g. a tag will be chased until the
    /// referenced object is no longer a tag).
    pub fn peel(&self, kind: ObjectType) -> Result<Object<'repo>, Error> {
        let mut raw = ptr::null_mut();
        unsafe {
            try_call!(raw::git_object_peel(&mut raw, &*self.raw(), kind));
            Ok(Binding::from_raw(raw))
        }
    }

    /// Get a short abbreviated OID string for the object
    ///
    /// This starts at the "core.abbrev" length (default 7 characters) and
    /// iteratively extends to a longer string if that length is ambiguous. The
    /// result will be unambiguous (at least until new objects are added to the
    /// repository).
    pub fn short_id(&self) -> Result<Buf, Error> {
        unsafe {
            let buf = Buf::new();
            try_call!(raw::git_object_short_id(buf.raw(), &*self.raw()));
            Ok(buf)
        }
    }

    /// Attempt to view this object as a commit.
    ///
    /// Returns `None` if the object is not actually a commit.
    pub fn as_commit(&self) -> Option<&Commit<'repo>> {
        self.cast(ObjectType::Commit)
    }

    /// Attempt to consume this object and return a commit.
    ///
    /// Returns `Err(self)` if this object is not actually a commit.
    pub fn into_commit(self) -> Result<Commit<'repo>, Object<'repo>> {
        self.cast_into(ObjectType::Commit)
    }

    /// Attempt to view this object as a tag.
    ///
    /// Returns `None` if the object is not actually a tag.
    pub fn as_tag(&self) -> Option<&Tag<'repo>> {
        self.cast(ObjectType::Tag)
    }

    /// Attempt to consume this object and return a tag.
    ///
    /// Returns `Err(self)` if this object is not actually a tag.
    pub fn into_tag(self) -> Result<Tag<'repo>, Object<'repo>> {
        self.cast_into(ObjectType::Tag)
    }

    /// Attempt to view this object as a tree.
    ///
    /// Returns `None` if the object is not actually a tree.
    pub fn as_tree(&self) -> Option<&Tree<'repo>> {
        self.cast(ObjectType::Tree)
    }

    /// Attempt to consume this object and return a tree.
    ///
    /// Returns `Err(self)` if this object is not actually a tree.
    pub fn into_tree(self) -> Result<Tree<'repo>, Object<'repo>> {
        self.cast_into(ObjectType::Tree)
    }

    /// Attempt to view this object as a blob.
    ///
    /// Returns `None` if the object is not actually a blob.
    pub fn as_blob(&self) -> Option<&Blob<'repo>> {
        self.cast(ObjectType::Blob)
    }

    /// Attempt to consume this object and return a blob.
    ///
    /// Returns `Err(self)` if this object is not actually a blob.
    pub fn into_blob(self) -> Result<Blob<'repo>, Object<'repo>> {
        self.cast_into(ObjectType::Blob)
    }

    /// Describes a commit
    ///
    /// Performs a describe operation on this commitish object.
    pub fn describe(&self, opts: &DescribeOptions)
                    -> Result<Describe, Error> {
        let mut ret = ptr::null_mut();
        unsafe {
            try_call!(raw::git_describe_commit(&mut ret, self.raw, opts.raw()));
            Ok(Binding::from_raw(ret))
        }
    }

    fn cast<T>(&self, kind: ObjectType) -> Option<&T> {
        assert_eq!(mem::size_of::<Object>(), mem::size_of::<T>());
        if self.kind() == Some(kind) {
            unsafe { Some(&*(self as *const _ as *const T)) }
        } else {
            None
        }
    }

    fn cast_into<T>(self, kind: ObjectType) -> Result<T, Object<'repo>> {
        assert_eq!(mem::size_of_val(&self), mem::size_of::<T>());
        if self.kind() == Some(kind) {
            Ok(unsafe {
                let other = ptr::read(&self as *const _ as *const T);
                mem::forget(self);
                other
            })
        } else {
            Err(self)
        }
    }
}

impl<'repo> Clone for Object<'repo> {
    fn clone(&self) -> Object<'repo> {
        let mut raw = ptr::null_mut();
        unsafe {
            let rc = raw::git_object_dup(&mut raw, self.raw);
            assert_eq!(rc, 0);
            Binding::from_raw(raw)
        }
    }
}

impl<'repo> Binding for Object<'repo> {
    type Raw = *mut raw::git_object;

    unsafe fn from_raw(raw: *mut raw::git_object) -> Object<'repo> {
        Object { raw: raw, _marker: marker::PhantomData, }
    }
    fn raw(&self) -> *mut raw::git_object { self.raw }
}

impl<'repo> Drop for Object<'repo> {
    fn drop(&mut self) {
        unsafe { raw::git_object_free(self.raw) }
    }
}
