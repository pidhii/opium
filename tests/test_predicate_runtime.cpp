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
    
    EXPECT_TRUE(prt.find(prt[opi::sym("X")]) == prt.find(prt[opi::sym("Z")]));
}

// Test value assignment
TEST_F(PredicateRuntimeTest, ValueAssignment) {
    opi::predicate_runtime prt;
    prt.assign(prt[opi::sym("X")], opi::sym("value1"));
    
    opi::value result = opi::nil;
    bool has_val = prt.get_value(opi::sym("X"), result);
    
    EXPECT_TRUE(has_val);
    EXPECT_TRUE(opi::equal(result, opi::sym("value1")));
}

// Test unification with values
TEST_F(PredicateRuntimeTest, UnificationWithValues) {
    opi::predicate_runtime prt;
    prt.assign(prt[opi::sym("X")], opi::sym("value1"));
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    
    opi::value y_val = opi::nil;
    bool y_has_val = prt.get_value(opi::sym("Y"), y_val);
    
    EXPECT_TRUE(y_has_val);
    EXPECT_TRUE(opi::equal(y_val, opi::sym("value1")));
}

// Test unification of variables and then assignment
TEST_F(PredicateRuntimeTest, UnificationAndAssignment) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    prt.assign(prt[opi::sym("X")], opi::sym("value2"));
    
    opi::value y_val = opi::nil;
    bool y_has_val = prt.get_value(opi::sym("Y"), y_val);
    
    EXPECT_TRUE(y_has_val);
    EXPECT_TRUE(opi::equal(y_val, opi::sym("value2")));
}

// Test equal method with simple values
TEST_F(PredicateRuntimeTest, EqualSimple) {
    opi::predicate_runtime prt;
    
    EXPECT_TRUE(prt.equal(opi::sym("a"), opi::sym("a")));
    EXPECT_FALSE(prt.equal(opi::sym("a"), opi::sym("b")));
}

// Test equal method with variables having the same representative
TEST_F(PredicateRuntimeTest, EqualVariablesSameRepresentative) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    
    EXPECT_TRUE(prt.equal(opi::sym("X"), opi::sym("Y")));
}

// Test equal method with variables having different representatives
TEST_F(PredicateRuntimeTest, EqualVariablesDifferentRepresentatives) {
    opi::predicate_runtime prt;
    
    EXPECT_FALSE(prt.equal(opi::sym("X"), opi::sym("Y")));
}

// Test equal method with a variable and a value
TEST_F(PredicateRuntimeTest, EqualVariableWithValue) {
    opi::predicate_runtime prt;
    prt.assign(prt[opi::sym("X")], opi::sym("value"));
    
    EXPECT_TRUE(prt.equal(opi::sym("X"), opi::sym("value")));
}

// Test equal method with complex structures containing variables
TEST_F(PredicateRuntimeTest, EqualComplexStructuresWithVariables) {
    opi::predicate_runtime prt;
    prt.unify(prt[opi::sym("X")], prt[opi::sym("Y")]);
    opi::value expr1 = opi::list(opi::sym("f"), opi::sym("X"), opi::sym("Y"));
    opi::value expr2 = opi::list(opi::sym("f"), opi::sym("Y"), opi::sym("X"));
    
    EXPECT_TRUE(prt.equal(expr1, expr2));
}

// Test equal method with complex structures containing different variables
TEST_F(PredicateRuntimeTest, EqualComplexStructuresDifferentVariables) {
    opi::predicate_runtime prt;
    opi::value expr1 = opi::list(opi::sym("f"), opi::sym("X"), opi::sym("X"));
    opi::value expr2 = opi::list(opi::sym("f"), opi::sym("Y"), opi::sym("Z"));
    
    EXPECT_FALSE(prt.equal(expr1, expr2));
}
