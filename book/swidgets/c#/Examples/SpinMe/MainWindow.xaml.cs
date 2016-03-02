using SWidgets;

namespace SpinMe
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            SSpinner spinner = SSpinner.Create(0);
            spinner.Width = 75;
            this.Container.Children.Add(spinner);
        }
    }
}