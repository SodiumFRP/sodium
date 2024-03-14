using System.Threading.Tasks;

namespace Sodium.Frp
{
    internal class Utilities
    {
        internal static async Task Yield() => await Task.Yield();
    }
}