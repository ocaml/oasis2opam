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
  fix (fun self n ->
      if n = 0 then leaf
      else frequency [1, leaf;
                      2, (let x = self (n/2) and y = self(n/2) in
                          oneof [map2 vor x y; map2 vand x y])]
    )
  |> sized
  |> QCheck.make ~print:Version.string_of_comparator

let rec comparator_size = function
  | VGreater _ | VGreaterEqual _ | VEqual _ | VLesser _ | VLesserEqual _ -> 1
  | VOr (a, b) | VAnd (a, b) -> comparator_size a + comparator_size b

let equivalent ~name f is_equivalent =
  QCheck.Test.make ~count:1000 ~name
    (QCheck.map (fun v -> (v, f v)) arbitrary_version_comparator)
    is_equivalent
  |> QCheck.Test.check_exn

let test_comparators a b test =
  let res =
    QCheck.Test.make_cell (QCheck.make arbitrary_version)
      (fun v -> test (OASISVersion.comparator_apply v a)
                     (OASISVersion.comparator_apply v b))
    |> QCheck.Test.check_cell in
  let open QCheck.TestResult in
  match res.state with
  | Success -> true
  | Failed _ | Error _ -> false

let is_equivalent (a, b) = (a = b) || test_comparators a b ( = )

let is_inverse (a, b) = test_comparators a b (fun a b -> a = not b)

let () =
  equivalent ~name:"comparator_reduce"
    Version.comparator_reduce is_equivalent;
  equivalent ~name:"complement_reduce"
    Version.complement_reduce is_inverse
