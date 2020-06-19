using Amazon.CDK;
using Amazon.CDK.AWS.APIGateway;
using Amazon.CDK.AWS.Lambda;

namespace CdkWorkshop
{
    public class ConstructTryStack : Stack
    {
        public ConstructTryStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            // Defines a new lambda resource
            var greeting = new Function(this, "GreetingHandler", new FunctionProps
            {
                Runtime = Runtime.NODEJS_10_X,
                Code = Code.FromAsset("src/lambda"),
                Handler = "greeting.handler"
            });

            var greetingWithCounter = new HitCounter(this, "GreetingHitCounter", new HitCounterProps
            {
                Downstream = greeting
            });

            new LambdaRestApi(this, "Endpoint", new LambdaRestApiProps
            {
                Handler = greetingWithCounter.Handler
            });
        }
    }
}