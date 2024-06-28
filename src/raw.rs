use crate::Index;
use std::{marker::PhantomData, num::NonZeroU32};

/// Trait for types that can be used as the generation of the seats in [`RawStadium`].
///
/// # Predictability
/// The behaviour of a stadium depends on whether or not the generation type is well-behaved. Well-
/// behaved means that the sequence of values created by starting with `FIRST` and then calling
/// `next` on the last value of the sequence to get the next value contains no repeated values.
///
/// When the generation type is well-behaved, it is guaranteed that [`Id`]s will be unique and dead
/// [`Id`]s won't be able to access new elements at the same index in the stadium.
///
/// All generation types are well-behaved, except for [`NoGen`].
pub trait Gen: Copy + PartialEq + Eq + PartialOrd + Ord {
    const FIRST: Self;

    /// Given the current generation, returns the next generation.
    fn next(self) -> Option<Self>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NoGen;
impl Gen for NoGen {
    const FIRST: Self = NoGen;

    #[inline(always)]
    fn next(self) -> Option<Self> {
        Some(NoGen)
    }
}

impl Gen for NonZeroU32 {
    const FIRST: Self = match NonZeroU32::new(1) {
        Some(x) => x,
        None => unreachable!(),
    };

    #[inline(always)]
    fn next(self) -> Option<Self> {
        self.checked_add(1)
    }
}

/// An identifier for an element in a stadium.
///
/// [`Id`]s are used to access elements in a stadium, and should only be used with the stadium that
/// they originated from. Using an [`Id`] with a stadium that did not create it will not cause
/// undefined behaviour, but it _will_ result in unpredictable behaviour.
///
/// [`Id`]s of elements that have already been removed from the stadium are called _dead_ [`Id`]s.
pub struct Id<T, I, G> {
    pub(crate) index: I,
    pub(crate) gen: G,
    _phantom_data: PhantomData<T>,
}

impl<T, I, G> Id<T, I, G>
where
    G: Gen,
{
    #[inline]
    pub(crate) fn new(index: I, gen: G) -> Self {
        Self {
            index,
            gen,
            _phantom_data: PhantomData,
        }
    }
}

impl<T, I, G> Clone for Id<T, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, I, G> Copy for Id<T, I, G>
where
    I: Index,
    G: Gen,
{
}

impl<T, I, G> PartialEq for Id<T, I, G>
where
    I: Index,
    G: Gen,
{
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.gen == other.gen
    }
}

impl<T, I, G> Eq for Id<T, I, G>
where
    I: Index,
    G: Gen,
{
}

impl<T, I, G> std::fmt::Debug for Id<T, I, G>
where
    I: std::fmt::Debug,
    G: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(G{:?})", self.index, self.gen)
    }
}

/// The state of an [`AliveSeat`].
#[derive(Debug, Clone)]
enum SeatState<T, I> {
    /// This seat is empty and might containt a pointer to the next empty seat on the stack.
    Empty(Option<I>),
    /// This seat contains a value.
    Occupied(T),
}

impl<T, I> SeatState<T, I>
where
    I: Index,
{
    #[inline]
    fn as_value(&self) -> Option<&T> {
        if let SeatState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn as_value_mut(&mut self) -> Option<&mut T> {
        if let SeatState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn into_value(self) -> Option<T> {
        if let SeatState::Occupied(value) = self {
            Some(value)
        } else {
            None
        }
    }

    #[inline]
    fn next_empty(&self) -> Option<I> {
        if let SeatState::Empty(next) = self {
            *next
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct AliveSeat<T, I, G> {
    gen: G,
    state: SeatState<T, I>,
}

#[derive(Debug, Clone)]
enum Seat<T, I, G> {
    /// This seat has ran out of generations and cannot be used anymore.
    Tombstone,
    /// This seat can still be used.
    Alive(AliveSeat<T, I, G>),
}

impl<T, I, G> Seat<T, I, G>
where
    G: Gen,
{
    #[inline]
    const fn new() -> Self {
        Self::Alive(AliveSeat {
            gen: G::FIRST,
            state: SeatState::Empty(None),
        })
    }

    #[inline]
    fn as_alive(&self) -> Option<&AliveSeat<T, I, G>> {
        if let Seat::Alive(seat) = self {
            Some(seat)
        } else {
            None
        }
    }

    #[inline]
    fn as_alive_mut(&mut self) -> Option<&mut AliveSeat<T, I, G>> {
        if let Seat::Alive(seat) = self {
            Some(seat)
        } else {
            None
        }
    }

    #[inline]
    fn into_alive(self) -> Option<AliveSeat<T, I, G>> {
        if let Seat::Alive(seat) = self {
            Some(seat)
        } else {
            None
        }
    }
}

/// A stadium is a data structure that associates elements with [`Id`]s generated upon insertion.
///
/// # How it Works
/// It does this by storing the elements in a [`Vec`] of 'seats', where each seat can be either
/// occupied or empty and contains a generation that is increased every time an element is removed
/// from it. This way, every element can be uniquely identified by the index of it's seat and it's
/// generation.
///
/// All empty seats contain the index of the next known empty seat, essentially forming a linked
/// list of all empty seats.
///
/// ## Insertion
/// Whenever an element is inserted into a stadium, a seat must be assigned to it. This is done by
/// first checking the list of known empty seats and trying to acquire one if it isn't empty, or by
/// creating a new seat if the list is empty. Then, the [`Id`] of the inserted element will be
/// the index of the seat and it's generation at the moment of insertion.
///
/// ## Removal
/// Whenever an element is removed from a stadium, the seat it's contained in becomes empty and
/// it's generation is increased by one. Then, the seat will point to the current head of the empty
/// seat list and become the new head.
///
/// # Limitations
/// - Stadiums have no way of telling whether an [`Id`] is valid or not, i.e. if it was created by
///   itself. This means that using [`Id`]s created by another stadium _will_ result in unpredictable
///   behaviour (but never _undefined_ behaviour).
#[derive(Debug, Clone)]
pub struct RawStadium<T, I, G> {
    /// The seats of this stadium.
    seats: Vec<Seat<T, I, G>>,
    /// How many seats are occupied.
    occupied: I,
    /// The index of the seat at the top of the empty seat stack.
    empty_seats_head: Option<I>,
}

impl<T, I, G> RawStadium<T, I, G>
where
    I: Index,
    G: Gen,
{
    /// Creates a new, empty stadium.
    pub const fn new() -> Self {
        Self {
            seats: Vec::new(),
            occupied: I::FIRST,
            empty_seats_head: None,
        }
    }

    /// Returns the number of elements in this stadium.
    #[inline]
    pub fn len(&self) -> usize {
        self.occupied.to_usize()
    }

    /// Whether this stadium is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets the seat at the given index.
    #[inline]
    fn get_seat(&self, index: I) -> Option<&Seat<T, I, G>> {
        self.seats.get(index.to_usize())
    }

    /// Gets the seat at the given index.
    #[inline]
    fn get_seat_mut(&mut self, index: I) -> Option<&mut Seat<T, I, G>> {
        self.seats.get_mut(index.to_usize())
    }

    /// Returns a reference to the element with the given [`Id`].
    #[inline]
    pub fn get(&self, id: Id<T, I, G>) -> Option<&T> {
        let seat = self.get_seat(id.index)?;
        let Seat::Alive(alive) = seat else {
            return None;
        };

        if id.gen != alive.gen {
            return None;
        }

        alive.state.as_value()
    }

    /// Returns a mutable reference to the element with the given [`Id`].
    #[inline]
    pub fn get_mut(&mut self, id: Id<T, I, G>) -> Option<&mut T> {
        let seat = self.get_seat_mut(id.index)?;
        let Seat::Alive(alive) = seat else {
            return None;
        };

        if id.gen != alive.gen {
            return None;
        }

        alive.state.as_value_mut()
    }

    /// Acquires a seat from the top of the empty seat list and returns it's index. More
    /// specifically, this will:
    /// - Pop the seat index from the empty seats list
    /// - Update the empty stack to point to the next empty seat
    #[must_use = "the seat will be left unusable if not managed correctly"]
    fn acquire_seat_from_empty_list(&mut self) -> Option<I> {
        let empty_seats_head_index = self.empty_seats_head.take()?;

        // SAFETY: seats in the empty seats list must exist
        let empty_seats_head =
            unsafe { self.get_seat_mut(empty_seats_head_index).unwrap_unchecked() };

        // SAFETY: all seats in the empty stack are alive and empty
        self.empty_seats_head = unsafe {
            empty_seats_head
                .as_alive()
                .unwrap_unchecked()
                .state
                .next_empty()
        };

        Some(empty_seats_head_index)
    }

    /// Acquires an available seat and returns its index. The seat at the index is guaranteed to be
    /// [`Alive`](Seat::Alive).
    #[inline]
    #[must_use = "the seat will be left unusable if not managed correctly"]
    fn acquire_seat(&mut self) -> I {
        self.acquire_seat_from_empty_list().unwrap_or_else(|| {
            self.seats.push(Seat::new());
            I::from_usize(self.seats.len() - 1).expect("number of elements fits in I (index type)")
        })
    }

    /// Inserts a value into this stadium and returns it's [`Id`].
    #[must_use = "the element can only be accessed directly through it's id"]
    pub fn insert(&mut self, value: T) -> Id<T, I, G> {
        self.occupied = self
            .occupied
            .next()
            .unwrap_or_else(|| panic!("exceeded maximum number of elements in the stadium"));
        let index = self.acquire_seat();

        // SAFETY: `acquire_seat` guarantees a seat exists at the given index and that it is alive
        let seat = unsafe {
            self.get_seat_mut(index)
                .unwrap_unchecked()
                .as_alive_mut()
                .unwrap_unchecked()
        };
        seat.state = SeatState::Occupied(value);

        Id::new(index, seat.gen)
    }

    /// Removes an value with the given `id` from this stadium and returns it.
    pub fn remove(&mut self, id: Id<T, I, G>) -> Option<T> {
        self.occupied = self.occupied.previous()?;
        let empty_seats_head = std::mem::replace(&mut self.empty_seats_head, Some(id.index));

        let seat = self.get_seat_mut(id.index)?;
        let Seat::Alive(alive) = seat else {
            self.occupied = self.occupied.next().unwrap();
            self.empty_seats_head = empty_seats_head;
            return None;
        };

        if (id.gen != alive.gen) || alive.state.as_value().is_none() {
            self.occupied = self.occupied.next().unwrap();
            self.empty_seats_head = empty_seats_head;
            return None;
        }

        // ok - seat is alive and contains a value
        let Some(new_gen) = alive.gen.next() else {
            let seat = std::mem::replace(seat, Seat::Tombstone);
            self.empty_seats_head = empty_seats_head;

            // SAFETY: the seat was already checked to be alive and non-empty
            return unsafe {
                Some(
                    seat.into_alive()
                        .unwrap_unchecked()
                        .state
                        .into_value()
                        .unwrap_unchecked(),
                )
            };
        };
        alive.gen = new_gen;

        let occupied = std::mem::replace(&mut alive.state, SeatState::Empty(empty_seats_head));
        // SAFETY: the seat was already checked to be non-empty
        unsafe { Some(occupied.into_value().unwrap_unchecked()) }
    }

    /// Clears this stadium. This is equivalent to [removing](Self::remove) every element, one
    /// by one.
    #[inline]
    pub fn clear(&mut self) {
        for index in 0..self.seats.len() {
            let index = I::from_usize(index).unwrap();
            if let Some(alive) = self.get_seat(index).unwrap().as_alive() {
                self.remove(Id::new(index, alive.gen));
            }
        }
    }

    /// Returns an iterator of `(Id, &Value)` tuples in this stadium.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Id<T, I, G>, &T)> {
        self.seats.iter().enumerate().filter_map(|(index, seat)| {
            let alive = seat.as_alive()?;
            alive
                .state
                .as_value()
                .map(|v| (Id::new(I::from_usize(index).unwrap(), alive.gen), v))
        })
    }

    /// Returns an iterator of `(Id, &mut Value)` tuples in this stadium.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Id<T, I, G>, &mut T)> {
        self.seats
            .iter_mut()
            .enumerate()
            .filter_map(|(index, seat)| {
                let alive = seat.as_alive_mut()?;
                alive
                    .state
                    .as_value_mut()
                    .map(|v| (Id::new(I::from_usize(index).unwrap(), alive.gen), v))
            })
    }

    /// Returns an iterator over the [`Id`]s of the elements of this stadium.
    #[inline]
    pub fn ids(&self) -> impl Iterator<Item = Id<T, I, G>> + '_ {
        self.seats.iter().enumerate().filter_map(|(index, seat)| {
            let alive = seat.as_alive()?;
            Some(Id::new(I::from_usize(index).unwrap(), alive.gen))
        })
    }

    /// Returns an iterator over references to the elements of this stadium.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.seats
            .iter()
            .filter_map(|seat| seat.as_alive()?.state.as_value())
    }

    /// Returns an iterator over mutable references to the elements of this stadium.
    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.seats
            .iter_mut()
            .filter_map(|seat| seat.as_alive_mut()?.state.as_value_mut())
    }
}

impl<T, I, G> Default for RawStadium<T, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T, I, G> std::ops::Index<Id<T, I, G>> for RawStadium<T, I, G>
where
    I: Index,
    G: Gen,
{
    type Output = T;

    #[inline]
    fn index(&self, id: Id<T, I, G>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, I, G> std::ops::IndexMut<Id<T, I, G>> for RawStadium<T, I, G>
where
    I: Index,
    G: Gen,
{
    #[inline]
    fn index_mut(&mut self, id: Id<T, I, G>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}
