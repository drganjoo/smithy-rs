/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package software.amazon.smithy.rust.codegen.client.smithy.endpoints

import software.amazon.smithy.rust.codegen.client.testutil.asSmithyModel

internal class CreateEndpointParamsTest {
    val model = """
        namespace com.example
        use smithy.rules#endpointRuleSet
        use smithy.rules#contextParam
        use smithy.rules#staticContextParams

        @endpointRuleSet({
            "version": "1.0",
            "rules": [],
            "parameters": {
                "Bucket": { "required": false, "type": "String" },
                "Region": { "required": false, "type": "String", "builtIn": "AWS::Region" },
                "DisableEverything": { "required": false, "type": "Boolean" }
            }
        })
        service MyService {
            operations: [TestOperation]
        }

        @staticContextParams({ "disableEverything": { value: true } })
        operation TestOperation {
            input: TestOperationInput
        }

        structure TestOperationInput {
            @contextParam(name: "Bucket")
            bucket: String
        }
    """.asSmithyModel()
//
//    @Test
//    fun `generate an operation with parameters wired properly`() {
//        val ctx = testCodegenContext(model)
// //        val ruleset =
//        val injector = EndpointParamsGenerator(
//            ctx,
//            model.expectShape(ShapeId.from("com.example#TestOperation"), OperationShape::class.java),
//            listOf(object : RulesEngineBuiltInResolver {
//                override fun defaultFor(parameter: Parameter, configRef: String): Writable? {
//                    if (parameter.builtIn.get() == "AWS::Region") {
//                        return writable { rust("""Some("test-region")""") }
//                    }
//                    return null
//                }
//            },
//            ),
//        )
//        val project = TestWorkspace.testProject()
//        project.unitTest {
//            rust(
//                """
//                    struct Input { bucket: Option<String> }
//                    let input = Input{ bucket: Some("my-bucket".to_string()) };
//                    """,
//            )
//            injector.section(OperationSection.MutateInput(listOf(), "input", "config"))(this)
//            rust(
//                """
//                    let endpoint_params = _endpoint_params.expect("endpoint params should be valid");
//                    assert_eq!(endpoint_params.bucket(), Some("my-bucket"));
//                    assert_eq!(endpoint_params.region(), Some("test-region"));
//                    assert_eq!(endpoint_params.disable_everything(), Some(true));
//                    """,
//            )
//        }.compileAndTest()
//    }
}