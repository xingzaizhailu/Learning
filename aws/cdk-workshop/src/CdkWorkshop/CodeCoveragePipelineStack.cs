using Amazon.CDK;
using Amazon.CDK.AWS.S3;
using Amazon.CDK.AWS.IAM;
using Amazon.CDK.AWS.CodeCommit;
using Amazon.CDK.AWS.CodeBuild;
using Amazon.CDK.AWS.CodePipeline;
using Amazon.CDK.AWS.CodePipeline.Actions;
using System.Collections.Generic;

using StageProps = Amazon.CDK.AWS.CodePipeline.StageProps;

namespace CdkWorkshop
{
    public class CodeCoveragePipelineStack : Stack
    {
        internal class Constants
        {
            public const string Project = "CodeCoverage";
            public const string BuildProject = "BuildProject";
            public const string TestReportGroup = "TestResults";
            public const string CodeCoverageReportGroup = "CoverletResults";
        }
        public CodeCoveragePipelineStack(Construct scope, string id, IStackProps props = null) : base(scope, id, props)
        {
            var buildProjectName = Constants.Project + Constants.BuildProject;
            var buildProject = new PipelineProject(this, buildProjectName, new PipelineProjectProps
            {
                ProjectName = buildProjectName,
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
                                "dotnet test --collect:\"XPlat Code Coverage\" --logger trx",
                                "xmlPath=$(find $CODEBUILD_SRC_DIR/Calculator.Tests/TestResults -name \"*.xml\")",
                                "reportgenerator -reports:$xmlPath -targetdir:$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults/"
                            }
                        }
                    },
                    ["reports"] = new Dictionary<string, object>
                    {
                        [Constants.TestReportGroup] = new Dictionary<string, object>
                        {
                            ["files"] = new string[]
                            {
                                "**/*.trx"
                            },
                            ["base-directory"] = "$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults",
                            ["file-format"] = "VisualStudioTrx"
                        },
                        [Constants.CodeCoverageReportGroup] = new Dictionary<string, object>
                        {
                            ["files"] = new string[]
                            {
                                "**/*.xml"
                            },
                            ["base-directory"] = "$CODEBUILD_SRC_DIR/Calculator.Tests/TestResults",
                            ["file-format"] = "CoberturaXml"
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

            var bucketName = "codepipeline-ap-southeast-2-822565979272";
            var testReportGroupName = buildProjectName + "-" + Constants.TestReportGroup;
            var testReportGroup = new CfnReportGroup(this, "TestReportGroup", new CfnReportGroupProps
            {
                Name = testReportGroupName,
                Type = "TEST",
                ExportConfig = new CfnReportGroup.ReportExportConfigProperty{
                    ExportConfigType = "S3",
                    S3Destination = new CfnReportGroup.S3ReportExportConfigProperty{
                        Bucket = bucketName,
                        Packaging = "NONE",
                        Path = "TestReports"
                    }
                }
            });

            var codeCoverageReportGroupName = buildProjectName + "-" + Constants.CodeCoverageReportGroup;
            var CodeCoverageReportGroup = new CfnReportGroup(this, "CodeCoverageReportGroup", new CfnReportGroupProps
            {
                Name = codeCoverageReportGroupName,
                Type = "CODE_COVERAGE",
                ExportConfig = new CfnReportGroup.ReportExportConfigProperty{
                    ExportConfigType = "S3",
                    S3Destination = new CfnReportGroup.S3ReportExportConfigProperty{
                        Bucket = bucketName,
                        Packaging = "NONE",
                        Path = "CodeCoverageReports"
                    }
                }
            });

            buildProject.AddToRolePolicy(
                new PolicyStatement(new PolicyStatementProps
                {
                    Effect = Effect.ALLOW,
                    Actions = new []
                    {
                        "codebuild:BatchPutCodeCoverages"
                    },
                    Resources = new []
                    {
                        Fn.Join("", new string[]
                        {
                            "arn:",
                            Fn.Ref("AWS::Partition"),
                            ":codebuild:",
                            Fn.Ref("AWS::Region"),
                            ":",
                            Fn.Ref("AWS::AccountId"),
                            ":report-group/",
                            buildProject.ProjectName,
                            "-*"
                        })
                    }
                })
            );

            var code = Repository.FromRepositoryName(this, "TestCoverageRepo", "TestCoverageRepo");
            var artifactsBucket = Bucket.FromBucketName(this, "BucketByName", bucketName);
            var sourceOutput = new Artifact_("sourceArtifact");
            var buildOutput = new Artifact_("buildArtifact");
            var pipelineName = Constants.Project + "Pipeline";
            new Pipeline(this, pipelineName, new PipelineProps
            {
                PipelineName = pipelineName,
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
                }
            });
        }
    }
}