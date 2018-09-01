namespace Sylvester
{
    public enum RunStatus
    {
        ErrorAllocatingInput,
        ErrorAllocatingOutput,
        ErrorExecuting,
        ErrorComputingGradient,
        Success
    }
}