//! A [`Stadium`] is a collection that associates [`Id`]s with values, like a map. The distinction,
//! however, is that a stadium is responsible for generating the id for a value and returning it on
//! insertion.
//!
//! Stadiums are useful for representing concepts like graphs or anything where references would be
//! cumbersome to use. They also provide good performance thanks to the possibility of storing all
//! elements next to each other in a buffer, which is cache friendly.
//!
//! The internal implementation of a stadium can be done in multiple ways, each with their pros and
//! cons:
//! - How are elements stored?
//! - Can elements be removed?
//! - Are ids guaranteed to be unique? i.e. if an element is removed, is it guaranteed that it's
//!   id won't ever be able to mess with another element in the same stadium? (_ABA problem_)
//! - Do removed elements reclaim memory?
//!
//! Given that so many different tradeoffs exist, the stadium therefore depends on a [strategy],
//! i.e. an internal implementation. The guarantees of a stadium depend on it's strategy.
//!
//! [strategy]: StrategyKind

#![deny(unsafe_op_in_unsafe_fn)]
#![deny(clippy::missing_safety_doc)]
#![warn(clippy::trivially_copy_pass_by_ref)]
#![warn(
    clippy::bool_to_int_with_if,
    clippy::borrow_as_ptr,
    clippy::case_sensitive_file_extension_comparisons,
    clippy::cast_lossless,
    clippy::cast_ptr_alignment,
    clippy::checked_conversions,
    clippy::cloned_instead_of_copied,
    clippy::copy_iterator,
    clippy::default_union_representation,
    clippy::deref_by_slicing,
    clippy::doc_link_with_quotes,
    clippy::empty_drop,
    clippy::empty_structs_with_brackets,
    clippy::enum_glob_use,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::filter_map_next,
    clippy::flat_map_option,
    clippy::float_cmp,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::fn_to_numeric_cast_any,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::ignored_unit_patterns,
    clippy::implicit_clone,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_digit_groups,
    clippy::large_stack_arrays,
    clippy::large_types_passed_by_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_assert,
    clippy::manual_instant_elapsed,
    clippy::manual_let_else,
    clippy::manual_ok_or,
    clippy::manual_ok_or,
    clippy::manual_string_new,
    clippy::map_unwrap_or,
    clippy::match_bool,
    clippy::mem_forget,
    clippy::mismatching_type_param_order,
    clippy::multiple_inherent_impl,
    clippy::mut_mut,
    clippy::mutex_atomic,
    clippy::needless_bitwise_bool,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::needless_for_each,
    clippy::needless_raw_string_hashes,
    clippy::needless_raw_strings,
    clippy::negative_feature_names,
    clippy::no_mangle_with_rust_abi,
    clippy::non_send_fields_in_send_ty,
    clippy::option_option,
    clippy::partial_pub_fields,
    clippy::ptr_cast_constness,
    clippy::range_minus_one,
    clippy::rc_mutex,
    clippy::redundant_else,
    clippy::redundant_feature_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::single_char_lifetime_names,
    clippy::single_match_else,
    clippy::string_add,
    clippy::string_add_assign,
    clippy::string_lit_chars_any,
    clippy::string_to_string,
    clippy::struct_field_names,
    clippy::transmute_ptr_to_ptr,
    clippy::trivially_copy_pass_by_ref,
    clippy::unnested_or_patterns,
    clippy::unreadable_literal,
    clippy::unsafe_derive_deserialize,
    clippy::unused_self,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    nonstandard_style
)]

// pub mod auxiliary;
pub mod strategy;

use std::marker::PhantomData;
use strategy::{Strategy, StrategyExtClear, StrategyExtMap, StrategyExtRemove, StrategyKind};

/// A unit struct used for untyped [`Id`]s.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Untyped;

/// The id of an element in a [`Stadium`].
#[repr(transparent)]
pub struct Id<T, S>
where
    S: StrategyKind,
{
    pub(crate) inner: S::Id,
    _phantom: PhantomData<*const T>,
}

impl<T, S> Id<T, S>
where
    S: StrategyKind,
{
    /// Maps an [`Id<T>`] to an [`Id<U>`]. Usually used with [`Stadium::map`].
    #[inline(always)]
    pub fn map<U>(id: Self) -> Id<U, S> {
        Id {
            inner: id.inner,
            _phantom: PhantomData,
        }
    }

    /// Untypes this id.
    #[inline(always)]
    pub fn untype(self) -> UntypedId<S> {
        Id::map(self)
    }
}

impl<T, S> Clone for Id<T, S>
where
    S: StrategyKind,
{
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, S> Copy for Id<T, S> where S: StrategyKind {}

impl<T, S> PartialEq for Id<T, S>
where
    S: StrategyKind,
    S::Id: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T, S> Eq for Id<T, S>
where
    S: StrategyKind,
    S::Id: Eq,
{
}

impl<T, S> std::fmt::Debug for Id<T, S>
where
    S: StrategyKind,
    S::Id: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// Alias of [`Id<Untyped, S>`].
pub type UntypedId<S> = Id<Untyped, S>;

/// A collection that associates [`Id`]s generated on insertion with values.
///
/// For more info, see the [crate root](crate).
pub struct Stadium<T, S>(S::Strategy<T>)
where
    S: StrategyKind;

impl<T, S> std::fmt::Debug for Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Stadium").field(&self.0).finish()
    }
}

impl<T, S> Stadium<T, S>
where
    S: StrategyKind,
{
    /// Creates a new, empty stadium.
    #[inline]
    pub fn new() -> Self {
        Self(S::Strategy::<T>::default())
    }

    /// Creates a stadium from an instantiated strategy.
    #[inline]
    pub fn from_strategy(strategy: S::Strategy<T>) -> Self {
        Self(strategy)
    }

    /// Returns the number of elements in this stadium.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether this stadium is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns a reference to the element with the given [`Id`].
    #[inline]
    pub fn get(&self, id: Id<T, S>) -> Option<&T> {
        self.0.get(unsafe { std::mem::transmute_copy(&id) })
    }

    /// Returns a reference to the element with the given [`UntypedId`].
    #[inline]
    pub fn get_untyped(&self, id: UntypedId<S>) -> Option<&T> {
        self.get(Id::map(id))
    }

    /// Returns a mutable reference to the element with the given [`Id`].
    #[inline]
    pub fn get_mut(&mut self, id: Id<T, S>) -> Option<&mut T> {
        self.0.get_mut(unsafe { std::mem::transmute_copy(&id) })
    }

    /// Returns a mutable reference to the element with the given [`UntypedId`].
    #[inline]
    pub fn get_mut_untyped(&mut self, id: UntypedId<S>) -> Option<&mut T> {
        self.get_mut(Id::map(id))
    }

    /// Inserts a value into this stadium and returns it's [`Id`].
    #[inline]
    #[must_use = "the element can only be accessed directly through it's id"]
    pub fn insert(&mut self, value: T) -> Id<T, S> {
        unsafe { std::mem::transmute_copy(&self.0.insert(value)) }
    }

    /// Inserts a sequence of values into this stadium and returns their [`Id`]s in a [`Vec`].
    #[inline]
    #[must_use = "the elements can only be accessed directly through their ids"]
    pub fn insert_sequence<I, C>(&mut self, values: I) -> C
    where
        I: IntoIterator<Item = T>,
        C: FromIterator<Id<T, S>>,
    {
        values.into_iter().map(|value| self.insert(value)).collect()
    }

    /// Returns an iterator of `(Id, &Value)` tuples in this stadium.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Id<T, S>, &T)> {
        self.0
            .iter()
            .map(|(id, value)| (unsafe { std::mem::transmute_copy(&id) }, value))
    }

    /// Returns an iterator of `(Id, &mut Value)` tuples in this stadium.
    #[inline]
    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (Id<T, <S::Strategy<T> as Strategy<T>>::Kind>, &mut T)> {
        self.0
            .iter_mut()
            .map(|(id, value)| (unsafe { std::mem::transmute_copy(&id) }, value))
    }

    /// Returns an iterator over the [`Id`]s of the elements of this stadium.
    #[inline]
    pub fn ids(&self) -> impl Iterator<Item = Id<T, S>> + '_ {
        self.iter().map(|(id, _)| id)
    }

    /// Returns an iterator over references to the elements of this stadium.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.iter().map(|(_, value)| value)
    }

    /// Returns an iterator over mutable references to the elements of this stadium.
    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.iter_mut().map(|(_, value)| value)
    }
}

/// A stadium
impl<T, S> Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: StrategyExtRemove<T>,
{
    /// Removes an value with the given `id` from this stadium and returns it.
    pub fn remove(&mut self, id: Id<T, S>) -> Option<T> {
        self.0.remove(unsafe { std::mem::transmute_copy(&id) })
    }
}

impl<T, S> Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: StrategyExtClear<T>,
{
    /// Clears this stadium. This is equivalent to removing every element.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl<T, S> Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: StrategyExtMap<T>,
{
    /// Maps a [`Stadium<T>`] to a [`Stadium<U>`] as long as `T` and `U` have the same size and
    /// alignment. This method will fail to compile if the requirements are not met.
    #[inline]
    pub fn map<U, F>(self, f: F) -> Stadium<U, S>
    where
        F: FnMut(T) -> U,
    {
        let mapped = self.0.map(f);
        let transmuted = unsafe { std::mem::transmute_copy(&mapped) };

        #[expect(clippy::mem_forget, reason = "we transmuted the value")]
        std::mem::forget(mapped);

        Stadium(transmuted)
    }
}

impl<T, S> Default for Stadium<T, S>
where
    S: StrategyKind,
{
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T, S> std::ops::Index<Id<T, S>> for Stadium<T, S>
where
    S: StrategyKind,
{
    type Output = T;

    #[inline]
    fn index(&self, id: Id<T, S>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, S> std::ops::IndexMut<Id<T, S>> for Stadium<T, S>
where
    S: StrategyKind,
{
    #[inline]
    fn index_mut(&mut self, id: Id<T, S>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T, S> Clone for Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T, S> PartialEq for Stadium<T, S>
where
    S: StrategyKind,
    S::Strategy<T>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

pub type BumpStadium<T, I> = Stadium<T, strategy::bump::Bump<I>>;
pub type RiskyStadium<T, I> = Stadium<T, strategy::risky::Risky<I>>;
pub type GenStadium<T, I, G> = Stadium<T, strategy::gen::Generational<I, G>>;
