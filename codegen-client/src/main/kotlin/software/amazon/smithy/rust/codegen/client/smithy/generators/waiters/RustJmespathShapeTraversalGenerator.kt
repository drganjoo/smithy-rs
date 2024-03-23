package software.amazon.smithy.rust.codegen.client.smithy.generators.waiters

import software.amazon.smithy.jmespath.ExpressionSerializer
import software.amazon.smithy.jmespath.JmespathExpression
import software.amazon.smithy.jmespath.ast.AndExpression
import software.amazon.smithy.jmespath.ast.BinaryExpression
import software.amazon.smithy.jmespath.ast.ComparatorExpression
import software.amazon.smithy.jmespath.ast.CurrentExpression
import software.amazon.smithy.jmespath.ast.ExpressionTypeExpression
import software.amazon.smithy.jmespath.ast.FieldExpression
import software.amazon.smithy.jmespath.ast.FilterProjectionExpression
import software.amazon.smithy.jmespath.ast.FlattenExpression
import software.amazon.smithy.jmespath.ast.FunctionExpression
import software.amazon.smithy.jmespath.ast.IndexExpression
import software.amazon.smithy.jmespath.ast.LiteralExpression
import software.amazon.smithy.jmespath.ast.MultiSelectHashExpression
import software.amazon.smithy.jmespath.ast.MultiSelectListExpression
import software.amazon.smithy.jmespath.ast.NotExpression
import software.amazon.smithy.jmespath.ast.ObjectProjectionExpression
import software.amazon.smithy.jmespath.ast.OrExpression
import software.amazon.smithy.jmespath.ast.ProjectionExpression
import software.amazon.smithy.jmespath.ast.SliceExpression
import software.amazon.smithy.jmespath.ast.Subexpression
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.rust.codegen.client.smithy.ClientCodegenContext
import software.amazon.smithy.rust.codegen.core.rustlang.RustType
import software.amazon.smithy.rust.codegen.core.rustlang.SafeNamer
import software.amazon.smithy.rust.codegen.core.rustlang.Writable
import software.amazon.smithy.rust.codegen.core.rustlang.asRef
import software.amazon.smithy.rust.codegen.core.rustlang.render
import software.amazon.smithy.rust.codegen.core.rustlang.rust
import software.amazon.smithy.rust.codegen.core.rustlang.rustBlock
import software.amazon.smithy.rust.codegen.core.rustlang.rustBlockTemplate
import software.amazon.smithy.rust.codegen.core.rustlang.withBlock
import software.amazon.smithy.rust.codegen.core.rustlang.writable
import software.amazon.smithy.rust.codegen.core.smithy.RuntimeType.Companion.preludeScope
import software.amazon.smithy.rust.codegen.core.smithy.isOptional
import software.amazon.smithy.rust.codegen.core.smithy.rustType
import software.amazon.smithy.rust.codegen.core.util.dq
import software.amazon.smithy.rust.codegen.core.util.orNull
import java.text.NumberFormat

/**
 * Contains information about the output of a visited [JmespathExpression].
 */
data class GeneratedExpression(
    val identifier: String,
    val outputShape: Shape? = null,
    val outputType: RustType,
    val output: Writable,
) {
    fun isStringOrEnum(): Boolean = outputType.isString() || outputShape?.isEnumShape == true

    fun convertToString(ident: String): GeneratedExpression =
        if (outputType.isString()) {
            this.copy(
                identifier = ident,
                output =
                    writable {
                        rust("let $ident = $identifier;")
                    },
            )
        } else {
            GeneratedExpression(
                identifier = ident,
                outputShape = null,
                outputType = RustType.String,
                output =
                    writable {
                        rust("let $ident = $identifier.to_string();")
                    },
            )
        }

    fun convertToStr(ident: String): GeneratedExpression =
        if (outputType.isStr()) {
            this.copy(
                identifier = ident,
                output =
                    writable {
                        rust("let $ident = $identifier;")
                    },
            )
        } else {
            GeneratedExpression(
                identifier = ident,
                outputShape = null,
                outputType = RustType.Reference(null, RustType.Opaque("str")),
                output =
                    writable {
                        rust("let $ident = $identifier.as_str();")
                    },
            )
        }

    fun convertToBoolValue(namer: SafeNamer): GeneratedExpression {
        var ident = this.identifier
        var out = this.output
        if (outputType is RustType.Reference) {
            val tmp = namer.safeName("tmp")
            out =
                writable {
                    output(this)
                    rust("let $tmp = *$identifier;")
                }
            ident = tmp
        }
        return copy(
            identifier = ident,
            outputType = RustType.Bool,
            output = out,
        )
    }
}

data class UnsupportedJmesPathException(private val msg: String) : RuntimeException(msg)

data class InvalidJmesPathTraversalException(private val msg: String) : RuntimeException(msg)

class RustJmespathShapeTraversalGenerator(
    codegenContext: ClientCodegenContext,
) {
    private val model = codegenContext.model
    private val symbolProvider = codegenContext.symbolProvider
    private val safeNamer = SafeNamer()

    fun generate(
        expr: JmespathExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        try {
            val result =
                when (expr) {
                    is ComparatorExpression -> generateComparator(expr, inputName, shape)
                    is ExpressionTypeExpression -> generateExpressionType()
                    is FunctionExpression -> generateFunction(expr, inputName, shape)
                    is FieldExpression -> generateField(expr, inputName, shape)
                    is IndexExpression -> generateIndex()
                    is LiteralExpression -> generateLiteral(expr)
                    is MultiSelectListExpression -> generateMultiSelectList(expr, inputName, shape)
                    is MultiSelectHashExpression -> generateMultiSelectHash()
                    is AndExpression -> generateAnd(expr, inputName, shape)
                    is OrExpression -> generateOr(expr, inputName, shape)
                    is NotExpression -> generateNot(expr, inputName, shape)
                    is ObjectProjectionExpression -> generateObjectProjection(expr, inputName, shape)
                    is FilterProjectionExpression -> generateFilterProjection(expr, inputName, shape)
                    is ProjectionExpression -> generateProjection(expr, inputName, shape)
                    is SliceExpression -> generateSlice()
                    is Subexpression -> generateSubexpression(expr, inputName, shape)
                    is CurrentExpression -> throw RuntimeException("current expression must be handled in each expression type that can have one")
                    else -> throw UnsupportedJmesPathException("${expr.javaClass.name} expression type not supported by smithy-rs")
                }
            return result.copy(
                output =
                    writable {
                        result.output(this)
                        if (debugMode) {
                            rust("// ${result.identifier} = ${ExpressionSerializer().serialize(expr)}")
                        }
                    },
            )
        } catch (ex: UnsupportedJmesPathException) {
            throw ex.copy(msg = "${ex.message}\nExpression: ${ExpressionSerializer().serialize(expr)}")
        } catch (ex: InvalidJmesPathTraversalException) {
            throw ex.copy(msg = "${ex.message}\nExpression: ${ExpressionSerializer().serialize(expr)}")
        }
    }

    private fun generateComparator(
        expr: ComparatorExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val left = generate(expr.left, inputName, shape)
        val right = generate(expr.right, inputName, shape)
        val ident = safeNamer.safeName("_cmp")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Bool,
            output =
                writable {
                    left.output(this)
                    right.output(this)
                    if (left.isStringOrEnum() && right.isStringOrEnum()) {
                        val leftStr =
                            left.convertToString(safeNamer.safeName("_str")).let { leftString ->
                                leftString.output(this)
                                leftString.convertToStr(safeNamer.safeName("_str"))
                            }.also { it.output(this) }
                        val rightStr =
                            right.convertToString(safeNamer.safeName("_str")).let { rightString ->
                                rightString.output(this)
                                rightString.convertToStr(safeNamer.safeName("_str"))
                            }.also { it.output(this) }
                        rust("let $ident = ${leftStr.identifier} ${expr.comparator} ${rightStr.identifier};")
                    } else if (left.outputType.isNumber() && right.outputType.isNumber()) {
                        val leftPrim = convertToNumberPrimitive(left.identifier, left.outputType, left.outputType)
                        val rightPrim = convertToNumberPrimitive(right.identifier, right.outputType, left.outputType)
                        rust("let $ident = ($leftPrim) ${expr.comparator} ($rightPrim);")
                    } else if (left.outputType.isBool() && right.outputType.isBool()) {
                        rust("let $ident = ${left.identifier} ${expr.comparator} ${right.identifier};")
                    } else {
                        throw UnsupportedJmesPathException("Comparison of ${left.outputType.render()} with ${right.outputType.render()} is not supported by smithy-rs")
                    }
                },
        )
    }

    private fun generateExpressionType(): GeneratedExpression {
        throw UnsupportedJmesPathException("Expression type expressions are not supported by smithy-rs")
    }

    private fun generateFunction(
        expr: FunctionExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val ident = safeNamer.safeName("_ret")
        return when (expr.name) {
            "length" -> {
                if (expr.arguments.size != 1) {
                    throw InvalidJmesPathTraversalException("Length function takes exactly one argument")
                }
                val arg = generate(expr.arguments[0], inputName, shape)
                if (!arg.outputType.isCollection() && !arg.outputType.isString()) {
                    throw InvalidJmesPathTraversalException("Argument to `length` function must be a collection or string type")
                }
                GeneratedExpression(
                    identifier = ident,
                    outputShape = null,
                    outputType = RustType.Integer(64),
                    output =
                        writable {
                            arg.output(this)
                            rust("let $ident = ${arg.identifier}.len() as i64;")
                        },
                )
            }

            "contains" -> {
                if (expr.arguments.size != 2) {
                    throw InvalidJmesPathTraversalException("Contains function takes exactly two arguments")
                }
                val left = generate(expr.arguments[0], inputName, shape)
                if (!left.outputType.isCollection() && !left.outputType.isString()) {
                    throw InvalidJmesPathTraversalException("First argument to `contains` function must be a collection or string type")
                }
                if (expr.arguments[1].isLiteralNull()) {
                    throw UnsupportedJmesPathException("Checking for null with `contains` is not supported in smithy-rs")
                }
                val right = generate(expr.arguments[1], inputName, shape)
                if (!right.outputType.isNumber() && !right.outputType.isString() && right.outputShape?.isEnumShape != true) {
                    throw UnsupportedJmesPathException("Checking for anything other than numbers, strings, or enums in the `contains` function is not supported in smithy-rs")
                }
                if (left.outputType.isString()) {
                    return GeneratedExpression(
                        identifier = ident,
                        outputShape = null,
                        outputType = RustType.Bool,
                        output =
                            writable {
                                left.output(this)
                                right.output(this)
                                if (right.outputType.isString()) {
                                    rust("let $ident = ${left.identifier}.contains(${right.identifier});")
                                } else {
                                    val tmp = safeNamer.safeName("_tmp")
                                    rust("let $tmp = ${right.identifier}.to_string();")
                                    rust("let $ident = ${left.identifier}.contains(&$tmp);")
                                }
                            },
                    )
                } else {
                    return GeneratedExpression(
                        identifier = ident,
                        outputShape = null,
                        outputType = RustType.Bool,
                        output =
                            writable {
                                left.output(this)
                                right.output(this)

                                // For now, convert everything to a string and then compare.
                                // This could be made much more efficient by only converting to string
                                // when absolutely necessary rather than always, but that requires
                                // much more sophisticated code generation. This additional work can
                                // be done if it becomes a performance problem.
                                val tmpRight = safeNamer.safeName("_tmp")
                                val tmpLeft = safeNamer.safeName("_tmp")
                                val toStringLeft = if (left.outputType.collectionValue().isString()) "" else ".to_string()"
                                val toStringRight = if (right.outputType.isString()) "" else ".to_string()"
                                val asStrLeft = if (toStringLeft.isEmpty()) "" else ".as_str()"
                                val asStrRight = if (toStringRight.isEmpty()) "" else ".as_str()"
                                rust(
                                    """
                                    let $tmpRight = ${right.identifier}$toStringRight;
                                    let $ident = ${left.identifier}.iter().any(|v| {
                                        let $tmpLeft = v$toStringLeft;
                                        $tmpLeft$asStrLeft == $tmpRight$asStrRight
                                    });
                                    """,
                                )
                            },
                    )
                }
            }

            else -> throw UnsupportedJmesPathException("The `${expr.name}` function is not supported by smithy-rs")
        }
    }

    private fun generateField(
        expr: FieldExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        if (shape is StructureShape) {
            val member =
                shape.getMember(expr.name).orNull()
                    ?: throw InvalidJmesPathTraversalException("Member `${expr.name}` doesn't exist on ${shape.id}")
            val memberSym = symbolProvider.toSymbol(member)

            val target = model.expectShape(member.target)
            val targetSym = symbolProvider.toSymbol(target)

            val ident = safeNamer.safeName("_fld")
            return GeneratedExpression(
                identifier = ident,
                outputShape = target,
                outputType = targetSym.rustType().asRef(),
                output =
                    writable {
                        if (memberSym.isOptional()) {
                            rust("let $ident = $inputName.${memberSym.name}.as_ref()?;")
                        } else {
                            rust("let $ident = &$inputName.${memberSym.name};")
                        }
                    },
            )
        } else {
            throw InvalidJmesPathTraversalException("Cannot look up fields in non-struct shapes")
        }
    }

    private fun generateIndex(): GeneratedExpression {
        throw UnsupportedJmesPathException("Index expressions are not supported by smithy-rs")
    }

    private fun generateLiteral(expr: LiteralExpression): GeneratedExpression {
        val outputType =
            when (expr.value) {
                is Boolean -> RustType.Reference(lifetime = null, member = RustType.Bool)
                is Double -> RustType.Reference(lifetime = null, member = RustType.Float(64))
                is String -> RustType.Reference(lifetime = null, member = RustType.Opaque("str"))
                null -> throw UnsupportedJmesPathException("Literal nulls are not supported by smithy-rs")
                else -> throw UnsupportedJmesPathException("Literal type ${expr.value.javaClass.name} is not supported by smithy-rs")
            }

        fun fmtFloating(floating: Number) =
            NumberFormat.getInstance().apply { minimumFractionDigits = 1 }.format(floating)

        val ident = safeNamer.safeName("_lit").uppercase()
        val output =
            writable {
                when (val value = expr.value) {
                    is Boolean -> rust("const $ident: &bool = &$value;")
                    is Double -> {
                        rust("const $ident: #T = &${fmtFloating(value)};", outputType)
                    }

                    is String -> rust("const $ident: &str = ${value.dq()};")
                    else -> throw RuntimeException("unreachable")
                }
            }
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = outputType,
            output = output,
        )
    }

    private fun generateMultiSelectList(
        expr: MultiSelectListExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val expressions =
            expr.expressions.map { subexpr ->
                generate(subexpr, inputName, shape)
            }
        // If we wanted to support mixed-types, we would need to use tuples, add tuple support to RustType,
        // and update supported functions such as `contains` to operate on tuples.
        for (pair in expressions.map { it.outputType }.windowed(2)) {
            if (pair[0] != pair[1]) {
                throw UnsupportedJmesPathException("Mixed-type multi-select lists are not supported by smithy-rs")
            }
        }

        val ident = safeNamer.safeName("_msl")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Vec(expressions[0].outputType),
            output =
                writable {
                    expressions.forEach { it.output(this) }
                    rust("let $ident = vec![${expressions.map { it.identifier }.joinToString(", ")}];")
                },
        )
    }

    private fun generateMultiSelectHash(): GeneratedExpression {
        throw UnsupportedJmesPathException("Multi-select hash expressions are not supported by smithy-rs")
    }

    private fun generateAnd(
        expr: AndExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression = generateBooleanOp(expr, "&&", inputName, shape)

    private fun generateOr(
        expr: OrExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression = generateBooleanOp(expr, "||", inputName, shape)

    private fun generateBooleanOp(
        expr: BinaryExpression,
        op: String,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val left = generate(expr.left, inputName, shape)
        val right = generate(expr.right, inputName, shape)
        if (!left.outputType.isBool() || !right.outputType.isBool()) {
            throw UnsupportedJmesPathException("Applying the `or` operation doesn't support non-boolean types in smithy-rs")
        }

        val ident = safeNamer.safeName("_bo")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Bool,
            output =
                writable {
                    val leftBool = left.convertToBoolValue(safeNamer).also { it.output(this) }
                    val rightBool = right.convertToBoolValue(safeNamer).also { it.output(this) }
                    rust("let $ident = ${leftBool.identifier} $op ${rightBool.identifier};")
                },
        )
    }

    private fun generateNot(
        expr: NotExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val inner = generate(expr.expression, inputName, shape)
        if (!inner.outputType.isBool()) {
            throw UnsupportedJmesPathException("Negation of a non-boolean type is not supported by smithy-rs")
        }

        val ident = safeNamer.safeName("_not")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Bool,
            output =
                writable {
                    inner.output(this)
                    rust("let $ident = !${inner.identifier};")
                },
        )
    }

    private fun generateProjection(
        expr: ProjectionExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val maybeFlatten = expr.left
        if (maybeFlatten !is FlattenExpression) {
            throw UnsupportedJmesPathException("Only projection expressions with flattens are supported by smithy-rs")
        }
        val left = generate(maybeFlatten.expression, inputName, shape)
        val leftTarget =
            when (val outputShape = left.outputShape) {
                is ListShape -> model.expectShape(outputShape.member.target)
                else -> throw InvalidJmesPathTraversalException("Left side of the flatten projection MUST resolve to a list or set shape")
            }
        val leftTargetSym = symbolProvider.toSymbol(leftTarget)

        // Short-circuit in the case where the projection is unnecessary
        if (left.outputType.isCollection() && expr.right is CurrentExpression) {
            return left
        }

        val right = generate(expr.right, "v", leftTarget)
        val projectionType = RustType.Vec(right.outputType.asRef())

        val ident = safeNamer.safeName("_prj")
        return GeneratedExpression(
            identifier = ident,
            outputShape = right.outputShape,
            outputType = projectionType,
            output =
                writable {
                    left.output(this)
                    rustBlock("let $ident = ${left.identifier}.iter().flat_map(") {
                        rustBlockTemplate(
                            "fn map(v: &#{Left}) -> #{Option}<#{Right}>",
                            *preludeScope,
                            "Left" to leftTargetSym,
                            "Right" to right.outputType,
                        ) {
                            right.output(this)
                            rust("Some(${right.identifier})")
                        }
                        rust("map")
                    }
                    rust(").collect::<Vec<_>>();")
                },
        )
    }

    private fun generateFilterProjection(
        expr: FilterProjectionExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val left = generate(expr.left, inputName, shape)
        if (!left.outputType.isList() && !left.outputType.isSet()) {
            throw UnsupportedJmesPathException("Filter projections can only be done on lists or sets in smithy-rs")
        }

        val leftTarget = model.expectShape((left.outputShape as ListShape).member.target)
        val leftTargetSym = symbolProvider.toSymbol(leftTarget)

        val right =
            if (expr.right is CurrentExpression) {
                left.copy(
                    outputType = left.outputType.collectionValue().asRef(),
                    output = writable {},
                )
            } else {
                generate(expr.right, "v", leftTarget)
            }

        val comparison = generate(expr.comparison, "_v", leftTarget)
        if (!comparison.outputType.isBool()) {
            throw InvalidJmesPathTraversalException("The filter expression comparison must result in a boolean")
        }

        val ident = safeNamer.safeName("_fprj")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Vec(right.outputType),
            output =
                writable {
                    left.output(this)
                    rust("let $ident = ${left.identifier}.iter()")
                    withBlock(".filter({", "})") {
                        rustBlock("fn filter(_v: &#T) -> Option<bool>", leftTargetSym) {
                            val toBool = comparison.convertToBoolValue(safeNamer).also { it.output(this) }
                            rust("Some(${toBool.identifier})")
                        }
                        rust("|v| filter(v).unwrap_or_default()")
                    }
                    if (expr.right !is CurrentExpression) {
                        withBlock(".flat_map({", "})") {
                            rustBlockTemplate(
                                "fn map(v: &#{Left}) -> #{Option}<#{Right}>",
                                *preludeScope,
                                "Left" to leftTargetSym,
                                "Right" to right.outputType,
                            ) {
                                right.output(this)
                                rust("Some(${right.identifier})")
                            }
                            rust("map")
                        }
                    }
                    rust(".collect::<Vec<_>>();")
                },
        )
    }

    private fun generateObjectProjection(
        expr: ObjectProjectionExpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val left = generate(expr.left, inputName, shape)
        if (!left.outputType.isMap()) {
            throw UnsupportedJmesPathException("Object projection is only supported on map types in smithy-rs")
        }
        if (left.outputShape == null) {
            throw UnsupportedJmesPathException("Object projection cannot be done on computed maps in smithy-rs")
        }

        val leftTarget = model.expectShape((left.outputShape as MapShape).value.target)
        val leftTargetSym = symbolProvider.toSymbol(leftTarget)

        val right =
            if (expr.right is CurrentExpression) {
                left.copy(
                    outputType = left.outputType.collectionValue().asRef(),
                    output = writable {},
                )
            } else {
                generate(expr.right, "v", leftTarget)
            }

        val ident = safeNamer.safeName("_oprj")
        return GeneratedExpression(
            identifier = ident,
            outputShape = null,
            outputType = RustType.Vec(right.outputType),
            output =
                writable {
                    left.output(this)
                    if (expr.right is CurrentExpression) {
                        rust("let $ident = ${left.identifier}.values().collect::<Vec<_>>();")
                    } else {
                        rustBlock("let $ident = ${left.identifier}.values().flat_map(") {
                            rustBlockTemplate(
                                "fn map(v: &#{Left}) -> #{Option}<#{Right}>",
                                *preludeScope,
                                "Left" to leftTargetSym,
                                "Right" to right.outputType,
                            ) {
                                right.output(this)
                                rust("Some(${right.identifier})")
                            }
                            rust("map")
                        }
                        rust(").collect::<Vec<_>>();")
                    }
                },
        )
    }

    private fun generateSlice(): GeneratedExpression {
        throw UnsupportedJmesPathException("Slice expressions are not supported by smithy-rs")
    }

    private fun generateSubexpression(
        expr: Subexpression,
        inputName: String,
        shape: Shape,
    ): GeneratedExpression {
        val left = generate(expr.left, inputName, shape)
        val right = generate(expr.right, left.identifier, left.outputShape!!)
        return GeneratedExpression(
            identifier = right.identifier,
            outputShape = right.outputShape,
            outputType = right.outputType,
            output =
                writable {
                    left.output(this)
                    right.output(this)
                },
        )
    }
}

private fun RustType.isList(): Boolean =
    this is RustType.Vec || this is RustType.Reference && this.member is RustType.Vec

private fun RustType.isSet(): Boolean =
    this is RustType.HashSet || this is RustType.Reference && this.member is RustType.HashSet

private fun RustType.isMap(): Boolean =
    this is RustType.HashMap || this is RustType.Reference && this.member is RustType.HashMap

private fun RustType.isCollection(): Boolean = isList() || isSet() || isMap()

private fun RustType.isString(): Boolean =
    this is RustType.String || isStr() ||
        this is RustType.Reference && this.member is RustType.String

private fun RustType.isStr(): Boolean =
    this is RustType.Reference && this.member is RustType.Opaque && this.member.name == "str"

private fun RustType.isNumber(): Boolean =
    this is RustType.Reference && (this.member is RustType.Integer || this.member is RustType.Float) ||
        this is RustType.Integer || this is RustType.Float

private fun RustType.isBool(): Boolean =
    this is RustType.Bool || this is RustType.Reference && this.member is RustType.Bool

private fun RustType.collectionValue(): RustType =
    when (this) {
        is RustType.Reference -> member.collectionValue()
        is RustType.Vec -> member
        is RustType.HashSet -> member
        is RustType.HashMap -> member
        else -> throw RuntimeException("expected collection type")
    }

private fun JmespathExpression.isLiteralNull(): Boolean = this == LiteralExpression.NULL

private fun convertToNumberPrimitive(
    name: String,
    currentType: RustType,
    desiredType: RustType,
): String {
    var result = name
    if (currentType is RustType.Reference) {
        result = "*$result"
    }
    var desiredRoot = desiredType
    if (desiredRoot is RustType.Reference) {
        desiredRoot = desiredRoot.member
    }
    return "$result as ${desiredRoot.render()}"
}
