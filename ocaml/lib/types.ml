module Val_type = Bindings.Extism_val_type

module Val = struct
  type t = (Bindings.Extism_val.t, [ `Struct ]) Ctypes.structured

  let t = Bindings.Extism_val.t

  let of_i32 (x : int32) : t =
    let u = Ctypes.make Bindings.Extism_val_union.t in
    Ctypes.(u @. Bindings.Extism_val_union.i32 <-@ x);
    let t = Ctypes.make Bindings.Extism_val.t in
    Ctypes.(t @. Bindings.Extism_val.ty <-@ Val_type.I32);
    Ctypes.(t @. Bindings.Extism_val.v <-@ u);
    t

  let of_i64 (x : int64) : t =
    let u = Ctypes.make Bindings.Extism_val_union.t in
    Ctypes.(u @. Bindings.Extism_val_union.i64 <-@ x);
    let t = Ctypes.make Bindings.Extism_val.t in
    Ctypes.(t @. Bindings.Extism_val.ty <-@ Val_type.I64);
    Ctypes.(t @. Bindings.Extism_val.v <-@ u);
    t

  let of_f32 (x : float) : t =
    let u = Ctypes.make Bindings.Extism_val_union.t in
    Ctypes.(u @. Bindings.Extism_val_union.f32 <-@ x);
    let t = Ctypes.make Bindings.Extism_val.t in
    Ctypes.(t @. Bindings.Extism_val.ty <-@ Val_type.F32);
    Ctypes.(t @. Bindings.Extism_val.v <-@ u);
    t

  let of_f64 (x : float) : t =
    let u = Ctypes.make Bindings.Extism_val_union.t in
    Ctypes.(u @. Bindings.Extism_val_union.f64 <-@ x);
    let t = Ctypes.make Bindings.Extism_val.t in
    Ctypes.(t @. Bindings.Extism_val.ty <-@ Val_type.F64);
    Ctypes.(t @. Bindings.Extism_val.v <-@ u);
    t

  let to_i32 t : int32 option =
    let ty = Ctypes.(t @. Bindings.Extism_val.ty) in
    let v = Ctypes.(t @. Bindings.Extism_val.v) in
    match Ctypes.( !@ ) ty with
    | Bindings.Extism_val_type.I32 ->
        Some Ctypes.(!@(!@v @. Bindings.Extism_val_union.i32))
    | _ -> None

  let to_i64 t : int64 option =
    let ty = Ctypes.(t @. Bindings.Extism_val.ty) in
    let v = Ctypes.(t @. Bindings.Extism_val.v) in
    match Ctypes.( !@ ) ty with
    | Bindings.Extism_val_type.I64 ->
        Some Ctypes.(!@(!@v @. Bindings.Extism_val_union.i64))
    | _ -> None

  let to_f32 t : float option =
    let ty = Ctypes.(t @. Bindings.Extism_val.ty) in
    let v = Ctypes.(t @. Bindings.Extism_val.v) in
    match Ctypes.( !@ ) ty with
    | Bindings.Extism_val_type.F32 ->
        Some Ctypes.(!@(!@v @. Bindings.Extism_val_union.f32))
    | _ -> None

  let to_f64 t : float option =
    let ty = Ctypes.(t @. Bindings.Extism_val.ty) in
    let v = Ctypes.(t @. Bindings.Extism_val.v) in
    match Ctypes.( !@ ) ty with
    | Bindings.Extism_val_type.F64 ->
        Some Ctypes.(!@(!@v @. Bindings.Extism_val_union.f64))
    | _ -> None

  let make_exn f x =
    match f x with Some x -> x | None -> Error.throw (`Msg "invalid type")

  let to_i32_exn = make_exn to_i32
  let to_i64_exn = make_exn to_i64
  let to_f32_exn = make_exn to_f32
  let to_f64_exn = make_exn to_f64
end

module Val_array = struct
  type t = Val.t Ctypes.CArray.t

  let get t i = Ctypes.CArray.get t i
  let set t i x = Ctypes.CArray.set t i x
  let length t = Ctypes.CArray.length t
  let ( .$[] ) = get
  let ( .$[]<- ) = set
end
