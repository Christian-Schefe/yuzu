use std::{any::Any, pin::Pin, task::Poll};

use futures::task::noop_waker_ref;
use gc_arena::{Collect, StaticCollect};

use crate::gc_interpreter::{
    Context,
    value::{FutureId, LocatedError, Value},
};

type ErasedOutput = Box<dyn Any>;

pub struct FutureArena {
    futures: Vec<Option<Pin<Box<dyn Future<Output = ErasedOutput>>>>>,
}

impl FutureArena {
    pub fn new() -> Self {
        Self {
            futures: Vec::new(),
        }
    }

    pub fn add_future<T: Future<Output = O> + 'static, O: Any + 'static>(
        &mut self,
        future: T,
    ) -> FutureId {
        let id = self.futures.len();
        self.futures.push(Some(Box::pin(async move {
            Box::new(future.await) as ErasedOutput
        })));
        id
    }

    pub fn poll_future(&mut self, id: FutureId) -> Option<Box<dyn Any>> {
        if let Some(entry) = self.futures.get_mut(id)
            && let Some(future) = entry.as_mut()
        {
            let waker = noop_waker_ref();
            let mut cx = std::task::Context::from_waker(waker);
            match future.as_mut().poll(&mut cx) {
                Poll::Ready(out) => {
                    *entry = None;
                    Some(out)
                }
                Poll::Pending => None,
            }
        } else {
            None
        }
    }
}

type FutureTransform = dyn for<'b> Fn(
    &Context<'b>,
    Vec<Value<'b>>,
    Box<dyn Any>,
) -> Result<Value<'b>, LocatedError<'b>>;

#[derive(Collect)]
#[collect(no_drop)]
pub struct FutureHandle<'a> {
    future: FutureId,
    args: Vec<Value<'a>>,
    transform: StaticCollect<Box<FutureTransform>>,
}

impl<'a> FutureHandle<'a> {
    pub fn new<F>(future_id: FutureId, values: Vec<Value<'a>>, transform: F) -> Self
    where
        F: for<'b> Fn(
                &Context<'b>,
                Vec<Value<'b>>,
                Box<dyn Any>,
            ) -> Result<Value<'b>, LocatedError<'b>>
            + 'static,
    {
        Self {
            future: future_id,
            args: values,
            transform: StaticCollect(Box::new(transform)),
        }
    }
    pub fn poll(&mut self, ctx: &Context<'a>) -> Option<Result<Value<'a>, Value<'a>>> {
        let mut arena = ctx.root.future_arena.borrow_mut();
        let res = arena.poll_future(self.future)?;
        Some((self.transform.0)(ctx, self.args.clone(), res))
    }
}
