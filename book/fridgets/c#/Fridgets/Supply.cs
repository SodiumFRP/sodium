using System;
using Sodium.Functional;

namespace Fridgets
{
    public class Supply
    {
        private readonly Impl impl;

        private Maybe<long> mId = Maybe.None;
        private readonly object idLock = new object();
        private Maybe<Supply> mChild1 = Maybe.None;
        private readonly object child1Lock = new object();
        private Maybe<Supply> mChild2 = Maybe.None;
        private readonly object child2Lock = new object();

        public Supply()
        {
            this.impl = new Impl();
        }

        private Supply(Impl impl)
        {
            this.impl = impl;
        }

        public long Get()
        {
            lock (this.idLock)
            {
                var result = this.mId.Match(
                    v => new { Result = v, Action = (Action)(() => { }) },
                    () =>
                    {
                        long id = this.impl.Alloc();
                        return new { Result = id, Action = (Action)(() => this.mId = Maybe.Some(id)) };
                    });
                result.Action();
                return result.Result;
            }
        }

        public Supply Child1()
        {
            lock (this.child1Lock)
            {
                var result = this.mChild1.Match(
                    v => new { Result = v, Action = (Action)(() => { }) },
                    () =>
                    {
                        Supply child1 = new Supply(this.impl);
                        return new { Result = child1, Action = (Action)(() => this.mChild1 = Maybe.Some(child1)) };
                    });
                result.Action();
                return result.Result;
            }
        }

        public Supply Child2()
        {
            lock (this.child2Lock)
            {
                var result = this.mChild2.Match(
                    v => new { Result = v, Action = (Action)(() => { }) },
                    () =>
                    {
                        Supply child2 = new Supply(this.impl);
                        return new { Result = child2, Action = (Action)(() => this.mChild2 = Maybe.Some(child2)) };
                    });
                result.Action();
                return result.Result;
            }
        }

        private class Impl
        {
            private long nextId;
            private readonly object nextIdLock = new object();

            public long Alloc()
            {
                lock (this.nextIdLock)
                {
                    return ++this.nextId;
                }
            }
        }
    }
}