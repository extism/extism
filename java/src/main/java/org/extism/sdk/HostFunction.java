package org.extism.sdk;

import com.sun.jna.Pointer;

import java.util.Arrays;

public class HostFunction {

    private final LibExtism.InternalExtismFunction callback;

    public final Pointer pointer;

    public final String name;

    public final LibExtism.ExtismValType[] params;

    public final LibExtism.ExtismValType[] returns;

    public final Pointer userData;

    public HostFunction(String name, LibExtism.ExtismValType[] params, LibExtism.ExtismValType[] returns, LibExtism.ExtismFunction f, Pointer userData) {

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

            /*LibExtism.ExtismVal.ByReference inputRef = new LibExtism.ExtismVal.ByReference();
            LibExtism.ExtismVal[] inputsVal = (LibExtism.ExtismVal[])inputRef.toArray(nInputs);

            var inputValue = new LibExtism.ExtismValUnion();
            inputValue.i32 = 14;

            inputsVal[0].value = inputValue;
            inputsVal[0].t = LibExtism.ExtismValType.I32.value;*/

            LibExtism.ExtismVal.ByReference outputRef = new LibExtism.ExtismVal.ByReference();
            LibExtism.ExtismVal[] outputsVal = (LibExtism.ExtismVal[])outputRef.toArray(nOutputs);

            LibExtism.ExtismVal[] hostFunctionsOutputsVal = (LibExtism.ExtismVal[])outputs.toArray(nOutputs);

            f.invoke(
                    new ExtismCurrentPlugin(currentPlugin),
                    inputs,
                    nInputs,
                    outputRef,
                    nOutputs,
                    userData
            );

            // System.out.println(outputsVal[0].value.i64);
            System.out.println(LibExtism.INSTANCE.extism_current_plugin_memory(currentPlugin).getString(outputsVal[0].value.i64));

            for (int i = 0; i < nOutputs; i++) {
                hostFunctionsOutputsVal[i].t = outputsVal[i].t;
                hostFunctionsOutputsVal[i].value.i64 = outputsVal[i].value.i64;
            }
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
