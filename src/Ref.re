module type REF = {
  type t('a);
  type m('t);

  let newRef: 'a => m(t('a));
  let read: t('a) => m('a);
  let write: ('a, t('a)) => m(unit);
  let modify: ('a => 'a, t('a)) => m(unit);
};

module MakeRef =
       (M: BsBastet.Interface.APPLICATIVE)
       : (REF with type m('a) = M.t('a)) => {
  type t('a) = ref('a);
  type m('t) = M.t('t);

  let newRef: 'a. 'a => M.t(t('a)) = a => M.pure(ref(a));

  let read: 'a. t('a) => M.t('a) = r => M.pure(r^);

  let write: 'a. ('a, t('a)) => M.t(unit) =
    (a, r) => {
      r := a;
      M.pure();
    };

  let modify: 'a. ('a => 'a, t('a)) => M.t(unit) =
    (f, r) => {
      r := f(r^);
      M.pure();
    };
};