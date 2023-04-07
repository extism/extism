namespace Extism.Sdk.Native;

using System;

/// <summary>
/// Represents errors that occur during calling Extism functions.
/// </summary>
public class ExtismException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="ExtismException"/> class.
    /// </summary>
    public ExtismException()
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="ExtismException"/> class with a specified error message.
    /// </summary>
    /// <param name="message">The message that describes the error .</param>
    public ExtismException(string message)
        : base(message)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="ExtismException"/> class
    /// with a specified error message and a reference to the inner exception
    /// that is the cause of this exception.
    /// </summary>
    /// <param name="message">The message that describes the error.</param>
    /// <param name="innerException">
    ///  The exception that is the cause of the current exception, or a null reference
    ///  (Nothing in Visual Basic) if no inner exception is specified.
    /// </param>
    public ExtismException(string message, Exception innerException)
        : base(message, innerException)
    {
    }
}