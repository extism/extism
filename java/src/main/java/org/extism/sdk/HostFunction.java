package org.extism.sdk;

import com.google.gson.JsonParser;
import com.sun.jna.Pointer;

import java.util.Arrays;

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
                         LibExtism.ExtismVal.ByReference outputs,
                         int nOutputs,
                         Pointer data) -> {
            var arraysOfInputs = ((LibExtism.ExtismVal[])inputs.toArray(nInputs));

            LibExtism.ExtismVal[] arraysOfOutputs = new LibExtism.ExtismVal[nOutputs];
            for (int i = 0; i < nOutputs; i++) {
                arraysOfOutputs[i] = new LibExtism.ExtismVal();
                arraysOfOutputs[i].t = arraysOfInputs[i].t;
                arraysOfOutputs[i].value = new LibExtism.ExtismValUnion();
            }

             f.invoke(
                    new ExtismCurrentPlugin(currentPlugin),
                     arraysOfInputs,
                     arraysOfOutputs,
                    new JsonParser().parse(data.getString(0))
            );

             //var tmp = (LibExtism.ExtismVal[])outputs.toArray(nOutputs);

             System.out.println(LibExtism.INSTANCE.extism_current_plugin_memory(currentPlugin).getString(arraysOfOutputs[0].value.i64));

             LibExtism.ExtismVal[] out = ((LibExtism.ExtismVal[])outputs.toArray(nOutputs));

             for (int i = 0; i < nOutputs; i++) {
                out[i].t = arraysOfOutputs[i].t;
                out[i].value = arraysOfOutputs[i].value;
             }

             System.out.println(outputs);
             System.out.println("Exit Host function");
        };

        this.pointer = LibExtism.INSTANCE.extism_function_new(
                this.name,
                Arrays.stream(this.params)
                        .mapToInt(typ -> typ.value)
                        .toArray(),
                this.params.length,
                Arrays.stream(this.returns)
                        .mapToInt(typ -> typ.value)
                        .toArray(),
                this.returns.length,
                this.callback,
                userData,
                null
        );
    }
}
