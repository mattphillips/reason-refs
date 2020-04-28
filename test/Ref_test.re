open Jest;
open Ref;

module Suite = (M: BsBastet.Interface.MONAD) => {
  module Ref = MakeRef(M);

  let (>>=): 'a 'b. (M.t('a), 'a => M.t('b)) => M.t('b) =
    (ma, f) => M.flat_map(ma, f);

  let (<$>): 'a 'b. ('a => 'b, M.t('a)) => M.t('b) =
    (ma, f) => M.map(ma, f);

  let make =
      (
        title: string,
        run:
          (
            assertion => unit,
            [< Expect.partial('a)] => assertion,
            M.t('a)
          ) =>
          unit,
      ) => {
    describe(
      title,
      () => {
        open Expect;

        let double = n => n * 2;

        testAsync("newRef -> read: returns value", onDone => {
          Ref.newRef(1) >>= Ref.read |> run(onDone, toEqual(1))
        });

        testAsync("newRef -> modify -> read: returns updated value", onDone =>
          Ref.newRef(1)
          >>= (r => (() => r) <$> Ref.modify(double, r))
          >>= Ref.read
          |> run(onDone, toEqual(2))
        );

        testAsync("newRef -> write -> read: returns written value", onDone =>
          Ref.newRef(1)
          >>= (r => (() => r) <$> Ref.write(2, r))
          >>= Ref.read
          |> run(onDone, toEqual(2))
        );

        testAsync(
          "newRef -> modify -> write -> read: returns written value", onDone =>
          Ref.newRef(1)
          >>= (r => (() => r) <$> Ref.modify(double, r))
          >>= (r => (() => r) <$> Ref.write(3, r))
          >>= Ref.read
          |> run(onDone, toEqual(3))
        );

        testAsync(
          "newRef -> write -> modify -> read: returns modified written value",
          onDone =>
          Ref.newRef(1)
          >>= (r => (() => r) <$> Ref.write(3, r))
          >>= (r => (() => r) <$> Ref.modify(double, r))
          >>= Ref.read
          |> run(onDone, toEqual(6))
        );
      },
    );
  };
};

module IO =
  Relude.IO.WithError({
    type t = string;
  });

module IOSuite = Suite(IO.Monad);

let run = (onDone, assertion) => {
  Expect.(
    Relude.IO.unsafeRunAsync(
      fun
      | Ok(value) => onDone(expect(value) |> assertion)
      | Error(_) => onDone(fail("Failed")),
    )
  );
};

IOSuite.make("IORef", run);

module AsyncSuite = Suite(Relude_AsyncData.Monad);

let run = (onDone, assertion, r) => {
  Expect.(
    Relude_AsyncData.getComplete(r)
    |> (
      fun
      | Some(value) => onDone(expect(value) |> assertion)
      | None => onDone(fail("Failed"))
    )
  );
};

AsyncSuite.make("AsyncDataRef", run);