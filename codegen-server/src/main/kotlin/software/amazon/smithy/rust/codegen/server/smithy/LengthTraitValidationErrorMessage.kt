/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package software.amazon.smithy.rust.codegen.server.smithy

import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.traits.LengthTrait
import software.amazon.smithy.rust.codegen.server.smithy.generators.idWithHashEscaped

fun LengthTrait.validationErrorMessage() =
    "Value with length {} at '{}' failed to satisfy constraint: Member must have length ${this.lengthDescription()}"

fun LengthTrait.shapeConstraintViolationDisplayMessage(shape: Shape) =
    "Value with length {} provided for '${shape.idWithHashEscaped()}' failed to satisfy constraint: Member must have length ${this.lengthDescription()}"

fun LengthTrait.lengthDescription() =
    if (this.min.isPresent && this.max.isPresent) {
        "between ${this.min.get()} and ${this.max.get()}, inclusive"
    } else if (this.min.isPresent) {
        "greater than or equal to ${this.min.get()}"
    } else {
        check(this.max.isPresent)
        "less than or equal to ${this.max.get()}"
    }
