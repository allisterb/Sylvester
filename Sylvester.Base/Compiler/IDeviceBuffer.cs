namespace Sylvester.Compiler
{
    public interface IDeviceBuffer
    {
        DeviceType DeviceType { get; }
        IShape Shape { get; }
    }
}