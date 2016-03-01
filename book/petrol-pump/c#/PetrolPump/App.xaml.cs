using System.Threading.Tasks;
using System.Windows;

namespace PetrolPump
{
    public partial class App
    {
        protected override void OnStartup(StartupEventArgs e)
        {
            // ensure unhandled exceptions on task pool threads aren't swallowed
            TaskScheduler.UnobservedTaskException += (sender, args) =>
            {
                if (!args.Observed)
                {
                    throw args.Exception;
                }
            };

            base.OnStartup(e);
        }
    }
}