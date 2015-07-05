open OUnit2
open QCheck
open OASISVersion

let version_of_int i = OASISVersion.version_of_string (string_of_int i)
let arbitrary_version = Arbitrary.(small_int >|= version_of_int)

let arbitrary_version_comparator =
  Arbitrary.(retry (
    small_int >>= fix_fuel [
      `Base (arbitrary_version >>= fun v ->
             among [VGreater v; VGreaterEqual v; VEqual v; VLesser v; VLesserEqual v]);
      `Rec (fun self ->
          self 2 >>= function
          | [x;y] -> among [VOr (x,y); VAnd (x,y)]
          | _ -> assert false
        )]))

let rec  comparator_size = function
  | VGreater _ | VGreaterEqual _ | VEqual _ | VLesser _ | VLesserEqual _ -> 1
  | VOr (a, b) | VAnd (a, b) -> comparator_size a + comparator_size b

let equivalent ~pp ~name ~size gen f is_equivalent =
  let pp (v1, v2) =
    Printf.sprintf "\ninput:(%s)\noutput:(%s)" (pp v1) (pp v2) in
  let size (v, _) = size v in
  mk_test ~n:1000 ~pp ~name ~size Arbitrary.(gen >|= fun v -> v, f v) is_equivalent

let is_equivalent (a, b) =
  (a = b) ||
  match check arbitrary_version (fun v -> OASISVersion.comparator_apply v a = OASISVersion.comparator_apply v b) with
  | Ok _ -> true
  | Failed _ | Error _ -> false

let is_inverse (a, b) =
  match check arbitrary_version (fun v -> OASISVersion.comparator_apply v a = not (OASISVersion.comparator_apply v b)) with
  | Ok _ -> true
  | Failed _ | Error _ -> false

let suite = QCheck_ounit.to_ounit_suite [
    equivalent ~pp:Version.string_of_comparator ~name:"comparator_reduce" ~size:comparator_size arbitrary_version_comparator
      Version.comparator_reduce is_equivalent;
    equivalent ~pp:Version.string_of_comparator ~name:"complement_reduce" ~size:comparator_size arbitrary_version_comparator
      Version.complement_reduce is_inverse;
  ]

let () = OUnit2.run_test_tt_main ("version" >::: suite)
