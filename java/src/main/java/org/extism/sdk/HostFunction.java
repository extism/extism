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

            // LibExtism.ExtismVal.ByReference outputRef = new LibExtism.ExtismVal.ByReference();
            // LibExtism.ExtismVal[] outputsVal = (LibExtism.ExtismVal[])outputRef.toArray(nOutputs);

            f.invoke(
                    new ExtismCurrentPlugin(currentPlugin),
                    inputs,
                    nInputs,
                    outputs,
                    nOutputs,
                    userData
            );

            //System.out.println(LibExtism.INSTANCE.extism_current_plugin_memory(currentPlugin).getString(outputsVal[0].value.i64));

            //LibExtism.ExtismVal[] hostFunctionsOutputsVal = (LibExtism.ExtismVal[])outputs.toArray(nOutputs);
            for (int i = 0; i < nOutputs; i++) {
                //System.out.println(hostFunctionsOutputsVal[i].t + ":" + outputsVal[i].t);
                //hostFunctionsOutputsVal[i] = outputsVal[i];
               //  hostFunctionsOutputsVal[i].t = outputsVal[i].t;
               // hostFunctionsOutputsVal[i].value.i64 = outputsVal[i].value.i64;
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
