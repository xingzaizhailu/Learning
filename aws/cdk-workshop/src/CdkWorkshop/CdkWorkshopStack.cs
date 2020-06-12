using Amazon.CDK;
using Amazon.CDK.AWS.Lambda;
using Amazon.CDK.AWS.SNS;
using Amazon.CDK.AWS.SNS.Subscriptions;

namespace CdkWorkshop
{
    public class CdkWorkshopStack : Stack
    {
        internal CdkWorkshopStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            // Function(scope, identical_id_in_scope, props)
            var handler = new Function(this, "SNSEventHandler", new FunctionProps()
            {
                Runtime = Runtime.DOTNET_CORE_3_1,
                Code = Code.FromAsset("src/MyFunction/src/MyFunction/bin/Release/netcoreapp3.1/publish"), // Code loaded from the "lambda" directory
                Handler = "MyFunction::MyFunction.Function::FunctionHandler" // file is "hello", function is hander
            });

            var topic = new Topic(this, "CdkWorkshopTopic");
            topic.AddSubscription(new LambdaSubscription(handler));
        }
    }
}
