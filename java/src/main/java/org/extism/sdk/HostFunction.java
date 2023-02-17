package org.extism.sdk;

import com.google.gson.JsonParser;
import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.PointerByReference;

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

            ExtismCurrentPlugin plugin = new ExtismCurrentPlugin(currentPlugin);
            int offs = plugin.alloc(4);
            Pointer mem = plugin.memory();
            mem.write(offs, "test".getBytes(), 0, 4);

          /*  Arrays.stream((LibExtism.ExtismVal[])inputs.toArray(nInputs)).forEach(r -> {
                System.out.println(Arrays.asList(r.t, r.v.i64));
            });

            LibExtism.ExtismVal[] outs = new LibExtism.ExtismVal[nOutputs];
            outs[0] = new LibExtism.ExtismVal();
            outs[0].v = new LibExtism.ExtismValUnion();
            outs[0].v.i64 = 1;
            outs[0].t = LibExtism.ExtismValType.I64.v;*/

            outputs.writeField("t", 1);
            outputs.writeField("v", (long)offs);
            //PointerByReference ptr = new PointerByReference();
      /*     Pointer ptrToFirst = outputs.getPointer();
           LibExtism.ExtismVal firstElement = new LibExtism.ExtismVal(ptrToFirst);
           LibExtism.ExtismVal[] array = (LibExtism.ExtismVal []) firstElement.toArray(1);
           array[0].v.i64 = 1;
           array[0].t = LibExtism.ExtismValType.I64.v;
           array[0].write();*/

          // outputs.write();

           // firstElement.write();

            // outputs.getPointer().setPointer(0, firstElement.getPointer());

            // outputs.v.i64 = inputs.v.i64;

            /*Arrays.stream((LibExtism.ExtismVal[])outputs.toArray(nInputs)).forEach(r -> {
                System.out.println(Arrays.asList(r.t, r.v.i64));
            });*/

            //outputs.read();
            //outputs.t = 1;
            //outputs.v.i64 = inputs.v.i64;
            //outputs.write();

          /*  LibExtism.ExtismVal[] outs = new LibExtism.ExtismVal[1];
            outs[0] = new LibExtism.ExtismVal();
            outs[0].t = 1;
            outs[0].v = new LibExtism.ExtismValUnion();
            outs[0].v.i64 = inputs.v.i64;*/

            /* f.invoke(
                    new ExtismCurrentPlugin(currentPlugin),
                     inputs,
                     outputs,
                    new JsonParser().parse(data.getString(0))
            );*/

           //  System.out.println(LibExtism.INSTANCE.extism_current_plugin_memory(currentPlugin).getString(outputs.v.i64));

             //System.out.println(inputs.getPointer().equals(outputs));

             System.out.println("Exit Host function");
        };

        this.pointer = LibExtism.INSTANCE.extism_function_new(
                this.name,
                Arrays.stream(this.params)
                        .mapToInt(typ -> typ.v)
                        .toArray(),
                this.params.length,
                Arrays.stream(this.returns)
                        .mapToInt(typ -> typ.v)
                        .toArray(),
                this.returns.length,
                this.callback,
                userData,
                null
        );
    }
}
