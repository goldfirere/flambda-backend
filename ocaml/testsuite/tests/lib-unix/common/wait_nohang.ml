(* TEST
 readonly_files = "reflector.ml";
 hassysthreads;
 {
   program = "${test_build_directory}/wait_nohang.byte";
   setup-ocamlc.byte-build-env;
   program = "${test_build_directory}/reflector.exe";
   all_modules = "reflector.ml";
   ocamlc.byte;
   include systhreads;
   program = "${test_build_directory}/wait_nohang.byte";
   all_modules = "wait_nohang.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   program = "${test_build_directory}/wait_nohang.opt";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/reflector.exe";
   all_modules = "reflector.ml";
   ocamlopt.byte;
   include systhreads;
   program = "${test_build_directory}/wait_nohang.opt";
   all_modules = "wait_nohang.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

let refl =
  Filename.concat Filename.current_dir_name "reflector.exe"

let () =
  let oc = Unix.open_process_out (refl ^ " -i2o") in
  let pid = Unix.process_out_pid oc in
  let (pid1, status1) = Unix.waitpid [WNOHANG] pid in
  assert (pid1 = 0);
  assert (status1 = WEXITED 0);
  output_string oc "aa\n"; close_out oc;
  let rec busywait () =
    let (pid2, status2) = Unix.waitpid [WNOHANG] pid in
    if pid2 = 0 then begin
      Unix.sleepf 0.001; busywait()
    end else begin
      assert (pid2 = pid);
      assert (status2 = WEXITED 0)
    end
  in busywait()
