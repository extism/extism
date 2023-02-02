package org.extism.sdk;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;

import java.nio.charset.StandardCharsets;


public class ExtismCurrentPlugin {
    public Pointer pointer;

    public ExtismCurrentPlugin(Pointer pointer) {
        this.pointer = pointer;
    }

    Pointer memory(long offs) {
        long length = LibExtism.INSTANCE.extism_current_plugin_memory_length(this.pointer, offs);
        Pointer data = LibExtism.INSTANCE.extism_current_plugin_memory(this.pointer);
        return data.share(offs, length);
    }

    int alloc(int n) {
        return LibExtism.INSTANCE.extism_current_plugin_memory_alloc(this.pointer, n);
    }

    void free(long offset) {
        LibExtism.INSTANCE.extism_current_plugin_memory_free(this.pointer, offset);
    }

    long memoryLength(long offset) {
        return LibExtism.INSTANCE.extism_current_plugin_memory_length(this.pointer, offset);
    }

    /**
     * Return a string from a host function
     * @param output - The output to set
     * @param s - The string to return
     */
    void  returnString(LibExtism.ExtismVal output, String s) {
        long offs = this.alloc(s.length());
        Pointer ptr = this.memory(offs);
        ptr.setString(0, s);
        output.value.i64 = offs;
        // TODO - not sure of t = 1
        output.t = 1;
    }

    /**
     * Return bytes from a host function
     * @param output - The output to set
     * @param b - The buffer to return
    void returnBytes(LibExtism.ExtismVal output, Pointer b) {
        var offs = this.memoryAlloc(b.length);
        this.memory(offs).set(b);
        output.v.i64 = offs;
    }  */


    /**
     * Get bytes from host function parameter
     * @param input - The input to read
     */
    Pointer inputBytes(LibExtism.ExtismVal input) {
        return this.memory(input.value.i64);
    }

    /**
     * Get string from host function parameter
     * @param input - The input to read
     */
    String inputString(LibExtism.ExtismVal input) {
        Pointer p = this.inputBytes(input);
        return p == null ? ""  : p.toString();
    }
}