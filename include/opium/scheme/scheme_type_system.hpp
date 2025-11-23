/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#pragma once

#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/value.hpp"


namespace opi {

std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode,
            const prolog &pl, const std::optional<prolog_guide_function> &guide);


/**
 * Generate implementation of a function template specialization
 *
 * \param ctx Current emitter-context
 * \param instantiation Function template instantiation from the TypeChecker
 * \param typetemplate Function template from the TypeChecker
 * \param ppbody Actual function body form the (preprocessed) input code
 * \param template_ctx Native context of the function template
 */
value
generate_function_template_body(scheme_emitter_context &ctx,
                                value instantiation, value type_template,
                                value ppbody);


value
instantiate_function_template(scheme_emitter_context &ctx, value type);

/**
 * Generate a distinctive name for a template instance using parameter types for
 * unique identification
 *
 * This function creates a string representation of a template instance in the format
 * `identifier<param1,param2,...>` with recursive handling of parameter types.
 * 
 * \param type The template instance type value
 * \param os The output stream to write the formatted name to
 */
void
pretty_template_instance_name(value type, std::ostream &os);

} // namespace opi
