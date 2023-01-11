package extism

import "core:c"

Ctx :: struct {
    ptr ^ExtismContext
}

newContex :: proc() -> Ctx {
    ctx: Ctx = {
        ptr: extism_context_new()
    }
    return ctx
}

freeContex :: proc(ctx: Ctx) {
    extism_context_free(ctx.ptr)
	ctx.pointer = nil
}

resetContext :: proc(ctx: Ctx) {
    extism_context_reset(ctx.ptr)
}
