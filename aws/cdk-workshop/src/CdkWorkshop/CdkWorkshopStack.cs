using System;
using Amazon.CDK;
using Amazon.CDK.AWS.Lambda;
using Amazon.CDK.AWS.Events;
using Amazon.CDK.AWS.Events.Targets;

namespace CdkWorkshop
{
    public class CdkWorkshopStack : Stack
    {
        internal CdkWorkshopStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            // Function(scope, identical_id_in_scope, props)
            var handler = new Function(this, "EventHandler", new FunctionProps()
            {
                Runtime = Runtime.DOTNET_CORE_3_1,
                Code = Code.FromAsset("src/MyFunction/src/MyFunction/bin/Release/netcoreapp3.1/publish"), // Code loaded from the "lambda" directory
                Handler = "MyFunction::MyFunction.Function::FunctionHandler" // file is "hello", function is hander
            });

            var eventPattern = new EventPattern();
            // "PipelineMetricsEventPattern"
            eventPattern.Source = new string[]{"aws.codepipeline"};
            eventPattern.DetailType = new string[]
            {
                "CodePipeline Pipeline Execution State Change",
                "CodePipeline Action Execution State Change",
                "CodePipeline Stage Execution State Change"
            };

            var rule = new Rule(this, "PipelineMetricsRule", new RuleProps
            {
                Description = "Push pipeline events to a lambda function.",
                RuleName = "PipelineMetricsRule",
                EventPattern = eventPattern,
                Enabled = true
            });
            rule.AddTarget(new LambdaFunction(handler, null));
        }
    }
}
