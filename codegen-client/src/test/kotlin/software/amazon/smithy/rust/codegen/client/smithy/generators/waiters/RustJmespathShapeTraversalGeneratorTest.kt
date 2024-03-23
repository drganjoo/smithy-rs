package software.amazon.smithy.rust.codegen.client.smithy.generators.waiters

import org.junit.jupiter.api.Test
import software.amazon.smithy.jmespath.JmespathExpression
import software.amazon.smithy.model.loader.ModelAssembler
import software.amazon.smithy.model.loader.ModelDiscovery
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.rust.codegen.client.testutil.clientIntegrationTest
import software.amazon.smithy.rust.codegen.core.rustlang.rust
import software.amazon.smithy.rust.codegen.core.rustlang.rustBlock
import software.amazon.smithy.rust.codegen.core.testutil.unitTest
import software.amazon.smithy.rust.codegen.core.util.lookup
import software.amazon.smithy.rust.codegen.core.util.outputShape

class RustJmespathShapeTraversalGeneratorTest {
    private fun testModel() =
        ModelAssembler().discoverModels().also { assembler ->
            val modelUrls = ModelDiscovery.findModels(javaClass.getResource("/META-INF/smithy/waiters-tests/manif3st"))
            for (url in modelUrls) {
                assembler.addImport(url)
            }
        }.assemble().unwrap()

    @Test
    fun itCompiles() {
        clientIntegrationTest(testModel()) { codegenContext, rustCrate ->
            val model = codegenContext.model
            val generate = { testName: String, expression: String ->
                val generator = RustJmespathShapeTraversalGenerator(codegenContext)
                val outputShape =
                    model.lookup<OperationShape>("com.test#GetBooleanLogic")
                        .outputShape(model)
                val outputSym = codegenContext.symbolProvider.toSymbol(outputShape)
                val parsed = JmespathExpression.parse(expression)
                val generated = generator.generate(parsed, "_output", outputShape)
                rustCrate.unitTest(testName) {
                    rust("// jmespath: $expression")
                    rust("// jmespath parsed: $parsed")
                    rustBlock("fn inner(_output: &#T) -> Option<#T>", outputSym, generated.outputType!!) {
                        generated.output(this)
                        rust("Some(${generated.identifier})")
                    }
                    rust("let output = #T::builder().build();", outputSym)
                    rust("let _ = inner(&output);")
                }
            }

            // Group 1: FieldExpression
            generate("traverse_field_primitives", "primitives")
            generate("traverse_field_lists", "lists")
            generate("traverse_field_two_dim_lists", "twoDimensionalLists")
            generate("traverse_field_maps", "maps")
            generate("traverse_field_sample_values", "sampleValues")
            generate("traverse_field_object_one", "objectOne")

            // Group 2: SubExpression
            generate("traverse_subexpression_bool", "primitives.boolean")
            generate("traverse_subexpression_string", "primitives.string")
            generate("traverse_subexpression_byte", "primitives.byte")
            generate("traverse_subexpression_short", "primitives.short")
            generate("traverse_subexpression_integer", "primitives.integer")
            generate("traverse_subexpression_long", "primitives.long")
            generate("traverse_subexpression_float", "primitives.float")
            generate("traverse_subexpression_double", "primitives.double")
            generate("traverse_subexpression_enum", "primitives.enum")
            generate("traverse_subexpression_int_enum", "primitives.intEnum")

            // Group 3: FlattenExpression
            generate("traverse_flatten_expression_shortcircuit", "lists.structs[]")
            generate("traverse_flatten_expression_no_shortcircuit", "lists.structs[].primitives.string")

            // Group 4: Literal types
            generate("traverse_literal_bool", "`true`")
            generate("traverse_literal_int", "`0`")
            generate("traverse_literal_float", "`1.5`")
            generate("traverse_literal_string", "`\"foo\"`")

            // Group 5: Functions
            generate("traverse_list_length", "length(lists.structs[])")
            generate("traverse_string_length", "length(primitives.string)")
            generate("traverse_string_contains", "contains(primitives.string, 'foo')")
            generate("traverse_strings_contains", "contains(lists.strings, 'foo')")
            generate("traverse_i16s_contains", "contains(lists.shorts, `0`)")
            generate("traverse_i32s_contains", "contains(lists.integers, `0`)")
            generate("traverse_i64s_contains", "contains(lists.longs, `0`)")
            generate("traverse_f32s_contains", "contains(lists.floats, `0`)")
            generate("traverse_f64s_contains", "contains(lists.doubles, `0`)")
            generate("traverse_enums_contains", "contains(lists.enums, 'one')")
            generate("traverse_intenums_contains", "contains(lists.intEnums, `0`)")
            generate("traverse_stringlit_contains_stringlit", "contains('foo', 'o')")
            generate("traverse_string_contains_stringlit", "contains(primitives.string, 'o')")
            generate("traverse_strings_contains_string", "contains(lists.strings, primitives.string)")
            generate("traverse_i32s_contains_i32", "contains(lists.integers, primitives.integer)")
            generate("traverse_i32s_contains_i16", "contains(lists.integers, primitives.short)")
            generate("traverse_f32s_contains_f32", "contains(lists.floats, primitives.float)")
            generate("traverse_f64s_contains_string", "contains(lists.floats, primitives.string)")

            // Group 6: Comparisons
            generate("traverse_compare_eq_boollit_w_boollit", "`true` == `true`")
            generate("traverse_compare_neq_boollit_w_boollit", "`true` != `true`")
            generate("traverse_compare_boollit_w_boollit", "`true` != `true`")
            generate("traverse_compare_bool_w_boollit", "primitives.boolean != `true`")
            generate("traverse_compare_bool_w_bool", "primitives.boolean == primitives.boolean")
            generate("traverse_compare_eq_integerlit_w_integerlit", "`0` == `0`")
            generate("traverse_compare_neq_integerlit_w_integerlit", "`0` != `0`")
            generate("traverse_compare_lt_integerlit_w_integerlit", "`0` < `0`")
            generate("traverse_compare_integer_w_integerlit", "primitives.integer != `0`")
            generate("traverse_compare_integer_w_integer", "primitives.integer == primitives.integer")
            generate("traverse_compare_float_w_integer", "primitives.float == primitives.integer")
            generate("traverse_compare_integer_w_float", "primitives.integer == primitives.float")
            generate("traverse_compare_eq_stringlit_w_stringlit", "'foo' == 'foo'")
            generate("traverse_compare_neq_stringlit_w_stringlit", "'bar' != 'foo'")
            generate("traverse_compare_string_w_stringlit", "primitives.string != 'foo'")
            generate("traverse_compare_string_w_string", "primitives.string == primitives.string")
            generate("traverse_compare_enum_w_stringlit", "primitives.enum == 'one'")
            generate("traverse_compare_enum_w_string", "primitives.enum == primitives.string")
            generate("traverse_compare_fn_w_number", "length(lists.structs[]) > `0`")

            // Group 7: Object projection
            generate("traverse_obj_projection_simple", "maps.booleans.*")
            generate("traverse_obj_projection_continued", "maps.structs.*.integer")
            generate("traverse_obj_projection_complex", "length(maps.structs.*.strings) > `0`")

            // Group 8: Filter projections
            generate("traverse_filter_projection_boollit", "lists.structs[?`true`]")
            generate("traverse_filter_projection_intcmp", "lists.structs[?integer > `0`]")
            generate("traverse_filter_projection_boollit_continued", "lists.structs[?`true`].integer")
            generate("traverse_filter_projection_intcmp_continued", "lists.structs[?integer > `0`].integer")

            // Group 9: Boolean operations
            generate("traverse_bool_lit_not", "!`true`")
            generate("traverse_bool_bool_not", "!(primitives.boolean)")
            generate("traverse_bool_lit_and_lit", "`true` && `false`")
            generate("traverse_bool_lit_or_lit", "`true` || `false`")
            generate("traverse_bool_bool_and_lit", "primitives.boolean && `true`")
            generate("traverse_bool_bool_or_lit", "primitives.boolean || `false`")
            generate("traverse_bool_bool_and_bool", "primitives.boolean && primitives.boolean")
            generate("traverse_bool_bool_or_bool", "primitives.boolean || primitives.boolean")

            // Group 10: Multi-select lists
            generate("traverse_literal_multiselect_1", "contains(lists.integers.[`1`, `2`, `3`], `1`)")
            generate("traverse_literal_multiselect_2", "contains(lists.integers.['foo', 'bar'], 'foo')")
            generate("traverse_literal_multiselect_3", "primitives.[integer, integer]")
            generate("traverse_literal_multiselect_4", "primitives.[boolean, boolean]")
            generate("traverse_literal_multiselect_5", "primitives.[string, string]")

            // All together now!
            generate("all_together_now", "(length(lists.structs[?!(integer < `0`) && integer >= `0` || `false`]) == `5`) == contains(lists.integers, length(maps.structs.*.strings))")
        }
    }
}
