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
