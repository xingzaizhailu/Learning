using Amazon.CDK;
using Amazon.CDK.AWS.S3;
using Amazon.CDK.AWS.CodeCommit;
using Amazon.CDK.AWS.CodeBuild;
using Amazon.CDK.AWS.CodePipeline;
using Amazon.CDK.AWS.CodePipeline.Actions;
using System.Collections.Generic;

using StageProps = Amazon.CDK.AWS.CodePipeline.StageProps;

namespace CdkWorkshop
{
    public class PipelineStack : Stack
    {
        internal class Constants
        {
            public const string Project = "CodeCoverage";
            public const string CodeBuildJob = "CodeBuildJob";
            public const string ReportGroupName = "coverlet-res";
        }
        public PipelineStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            var code = Repository.FromRepositoryName(this, "TestCoverageRepo",
                "TestCoverageRepo");
            var artifactsBucket = Bucket.FromBucketName(this, "BucketByName", "codepipeline-ap-southeast-2-822565979272");

            var buildProject = new PipelineProject(this, "buildProject", new PipelineProjectProps
            {
                ProjectName = Constants.Project + "BuildProject",
                BuildSpec = BuildSpec.FromObject(new Dictionary<string, object>
                {
                    ["version"] = "0.2",
                    ["phases"] = new Dictionary<string, object>
                    {
                        ["install"] = new Dictionary<string, object>
                        {
                            ["runtime-versions"] = new Dictionary<string, object>
                            {
                                ["dotnet"] = "latest"
                            },
                            ["commands"] = new string[]
                            {
                                "dotnet tool install -g dotnet-reportgenerator-globaltool",
                                "export PATH=\"$PATH:/root/.dotnet/tools\""
                            }
                        },
                        ["build"] = new Dictionary<string, object>
                        {
                            ["commands"] = new string[]
                            {
                                "dotnet build",
                                "dotnet test --collect:\"XPlat Code Coverage\"",
                                "xmlPath=$(find $CODEBUILD_SRC_DIR/Calculator.Tests/TestResults -name \"*.xml\")",
                                "reportgenerator -reports:$xmlPath -targetdir:$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults/"
                            }
                        }
                    },
                    ["reports"] = new Dictionary<string, object>
                    {
                        [Constants.ReportGroupName] = new Dictionary<string, object>
                        {
                            ["files"] = new string[]
                            {
                                "**/*"
                            },
                            ["base-directory"] = "$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults",
                            ["file-format"] = "VisualStudioTrx"
                        }
                    },
                    ["artifacts"] = new Dictionary<string, object>
                    {
                        ["files"] = new string[]
                        {
                            "**/*"
                        },
                        ["name"] = "coverlet-$(date +%Y-%m-%d)",
                        ["base-directory"] = "$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults/"
                    }
                }),
                Environment = new BuildEnvironment
                {
                    BuildImage = LinuxBuildImage.AMAZON_LINUX_2_3
                }
            });

            // new ReportGroup(this, "CodeCoverageReportGroup", new ReportGroupProps
            // {
            //     ReportGroupName = Constants.ReportGroupName,
            //     ExportBucket = artifactsBucket,
            //     ZipExport = false
            // });

            var sourceOutput = new Artifact_("sourceArtifact");
            var buildOutput = new Artifact_("buildArtifact");

            new Pipeline(this, "Pipeline", new PipelineProps
            {

                PipelineName = Constants.Project + "Pipeline",
                ArtifactBucket = artifactsBucket,
                Stages = new[]
                {
                    new StageProps
                    {
                        StageName = "Source",
                        Actions = new []
                        {
                            new CodeCommitSourceAction(new CodeCommitSourceActionProps
                            {
                                ActionName = "SourceAction",
                                Repository = code,
                                Output = sourceOutput
                            })
                        }
                    },
                    new StageProps
                    {
                        StageName = "Build",
                        Actions = new []
                        {
                            new CodeBuildAction(new CodeBuildActionProps
                            {
                                ActionName = "BuildAction",
                                Project = buildProject,
                                Input = sourceOutput,
                                Outputs = new [] { buildOutput }
                            })
                        }
                    },
                    // new StageProps
                    // {
                    //     StageName = "Deploy",
                    //     Actions = new []
                    //     {
                    //     }
                    // }
                }
            });

        }
    }
}