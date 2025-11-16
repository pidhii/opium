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


#include "opium/predicate_runtime.hpp"

#include <gtest/gtest.h>


// Test fixture for predicate_runtime tests
class PredicateRuntimeTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Common setup code if needed
    }

    void TearDown() override {
        // Common cleanup code if needed
    }
};

// Test variable unification
TEST_F(PredicateRuntimeTest, VariableUnification) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    prt.unify(prt[opi::sym("Y")], prt[opi::sym("Z")]);
    
    EXPECT_TRUE(find(prt[opi::sym("X")]) == find(prt[opi::sym("Z")]));
}

// Test value assignment
TEST_F(PredicateRuntimeTest, ValueAssignment) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], make_term(opi::sym("value1")));
    
    opi::value result = opi::nil;
    bool has_val = get_value(prt[opi::sym("X")], result);
    
    EXPECT_TRUE(has_val);
    EXPECT_TRUE(opi::equal(result, opi::sym("value1")));
}

// Test unification with values
TEST_F(PredicateRuntimeTest, UnificationWithValues) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], make_term(opi::sym("value1")));
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    
    opi::value y_val = opi::nil;
    bool y_has_val = get_value(prt[opi::sym("Y")], y_val);
    
    EXPECT_TRUE(y_has_val);
    EXPECT_TRUE(opi::equal(y_val, opi::sym("value1")));
}

// Test unification of variables and then assignment
TEST_F(PredicateRuntimeTest, UnificationAndAssignment) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    prt.unify(prt[opi::sym("X")], make_term(opi::sym("value2")));
    
    opi::value y_val = opi::nil;
    bool y_has_val = get_value(prt[opi::sym("Y")], y_val);
    
    EXPECT_TRUE(y_has_val);
    EXPECT_TRUE(opi::equal(y_val, opi::sym("value2")));
}
