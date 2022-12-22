type t = { mutable pointer : unit Ctypes.ptr }

let create () =
  let ptr = Bindings.extism_context_new () in
  let t = { pointer = ptr } in
  Gc.finalise (fun { pointer } -> Bindings.extism_context_free pointer) t;
  t

let free ctx =
  let () = Bindings.extism_context_free ctx.pointer in
  ctx.pointer <- Ctypes.null

let reset ctx = Bindings.extism_context_reset ctx.pointer

let%test "test context" =
  let ctx = create () in
  reset ctx;
  free ctx;
  true
