using Amazon.CDK;

namespace CdkWorkshop
{
    sealed class Program
    {
        public static void Main(string[] args)
        {
            var app = new App();

            new CdkWorkshopStack(app, "CdkWorkshopStack");
            new ConstructTryStack(app, "ConstructTryStack");

            app.Synth();
        }
    }
}
