package org.extism.sdk;

import com.google.gson.JsonParser;
import com.sun.jna.Pointer;

import java.util.Arrays;
import java.util.Optional;

public class HostFunction {

    private final LibExtism.InternalExtismFunction callback;

    public final Pointer pointer;

    public final String name;

    public final LibExtism.ExtismValType[] params;

    public final LibExtism.ExtismValType[] returns;

    public final Pointer userData;

    public HostFunction(String name, LibExtism.ExtismValType[] params, LibExtism.ExtismValType[] returns, ExtismFunction f, Pointer userData) {

        this.name = name;
        this.params = params;
        this.returns = returns;
        this.userData = userData;
        this.callback = (Pointer currentPlugin,
                         LibExtism.ExtismVal.ByReference inputs,
                         int nInputs,
                         LibExtism.ExtismVal.ByReference outs,
                         int nOutputs,
                         Pointer data) -> {

            LibExtism.ExtismVal[] outputs = (LibExtism.ExtismVal []) outs.toArray(nOutputs);

            f.invoke(
                    new ExtismCurrentPlugin(currentPlugin),
                    (LibExtism.ExtismVal []) inputs.toArray(nInputs),
                    outputs,
                    data == null ? Optional.empty() : Optional.of(new JsonParser().parse(data.getString(0)))
            );

            for (LibExtism.ExtismVal output : outputs) {
                convertOutput(output, output);
            }
        };

        this.pointer = LibExtism.INSTANCE.extism_function_new(
                this.name,
                Arrays.stream(this.params).mapToInt(r -> r.v).toArray(),
                this.params.length,
                Arrays.stream(this.returns).mapToInt(r -> r.v).toArray(),
                this.returns.length,
                this.callback,
                userData,
                null
        );
    }

    void convertOutput(LibExtism.ExtismVal original, LibExtism.ExtismVal fromHostFunction) {
        if (fromHostFunction.t != original.t)
            throw new ExtismException(String.format("Output type mismatch, got %d but expected %d", fromHostFunction.t, original.t));

        if (fromHostFunction.t == LibExtism.ExtismValType.I32.v) {
            original.v.setType(Integer.TYPE);
            original.v.i32 = fromHostFunction.v.i32;
        } else if (fromHostFunction.t == LibExtism.ExtismValType.I64.v) {
            original.v.setType(Long.TYPE);
            original.v.i64 = fromHostFunction.v.i64;
        } else if (fromHostFunction.t == LibExtism.ExtismValType.F32.v) {
            original.v.setType(Float.TYPE);
            original.v.f32 = fromHostFunction.v.f32;
        } else if (fromHostFunction.t == LibExtism.ExtismValType.F64.v) {
            original.v.setType(Double.TYPE);
            original.v.f64 = fromHostFunction.v.f64;
        } else
            throw new ExtismException(String.format("Unsupported return type: %s", original.t));
    }
}
