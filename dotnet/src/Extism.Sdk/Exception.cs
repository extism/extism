namespace Extism.Sdk.Native;

using System;

public class ExtismException : Exception
{
    public ExtismException()
    {
    }

    public ExtismException(string message)
        : base(message)
    {
    }

    public ExtismException(string message, Exception err)
        : base(message, err)
    {
    }
}