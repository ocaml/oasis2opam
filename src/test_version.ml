open Printf
open OASISVersion

let version_of_int i = OASISVersion.version_of_string (string_of_int i)
let arbitrary_version = QCheck.Gen.(small_int >|= version_of_int)

let arbitrary_version_comparator =
  let open QCheck.Gen in
  let leaf =
    arbitrary_version >>= fun v ->
    oneofl [VGreater v; VGreaterEqual v; VEqual v;
            VLesser v; VLesserEqual v] in
  let vor x y = VOr(x,y) in
  let vand x y = VAnd(x,y) in
  let sz =
    fix (fun self n ->
        if n = 0 then leaf
        else frequency [1, leaf;
                        2, (let x = self (n/2) and y = self(n/2) in
                            oneof [map2 vor x y; map2 vand x y])]
      ) in
  sized sz

let rec comparator_size = function
  | VGreater _ | VGreaterEqual _ | VEqual _ | VLesser _ | VLesserEqual _ -> 1
  | VOr (a, b) | VAnd (a, b) -> comparator_size a + comparator_size b

let equivalent ~name f is_equivalent fmt =
  let c = QCheck.Gen.map (fun v -> (v, f v)) arbitrary_version_comparator in
  let print (v, v') = sprintf fmt (Version.string_of_comparator v)
                        (Version.string_of_comparator v') in
  let rec shrink (v, _) g =
    match v with
    | VGreater _ | VGreaterEqual _ | VEqual _ | VLesser _ | VLesserEqual _ -> ()
    | VOr (a, b) | VAnd (a, b) ->
       let a = (a, f a) and b = (b, f b) in
       g a;
       g b;
       shrink a g;
       shrink b g in
  let small (v, _) = comparator_size v in
  let a = QCheck.make c ~print ~shrink ~small in
  let t = QCheck.Test.make ~count:1000 ~name a is_equivalent in
  QCheck.Test.check_exn t

let test_comparators a b test =
  let cell =
    QCheck.Test.make_cell (QCheck.make arbitrary_version)
      (fun v -> test (OASISVersion.comparator_apply v a)
                  (OASISVersion.comparator_apply v b)) in
  let res = QCheck.Test.check_cell cell in
  let open QCheck.TestResult in
  match res.state with
  | Success -> true
  | Failed _ | Error _ -> false

let is_equivalent (a, b) = (a = b) || test_comparators a b ( = )

let is_inverse (a, b) = test_comparators a b (fun a b -> a = not b)

let () =
  equivalent ~name:"comparator_reduce"
    Version.comparator_reduce is_equivalent "{%s} ≡ {%s}";
  equivalent ~name:"complement_reduce"
    Version.complement_reduce is_inverse "{%s} ≡ ¬{%s}"
