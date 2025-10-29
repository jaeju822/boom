// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "VTestDriver.h"
#include "VTestDriver__Syms.h"
#include "verilated_dpi.h"

//============================================================
// Constructors

VTestDriver::VTestDriver(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new VTestDriver__Syms(contextp(), _vcname__, this)}
    , __PVT____024unit{vlSymsp->TOP.__PVT____024unit}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
}

VTestDriver::VTestDriver(const char* _vcname__)
    : VTestDriver(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

VTestDriver::~VTestDriver() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void VTestDriver___024root___eval_debug_assertions(VTestDriver___024root* vlSelf);
#endif  // VL_DEBUG
void VTestDriver___024root___eval_static(VTestDriver___024root* vlSelf);
void VTestDriver___024root___eval_initial(VTestDriver___024root* vlSelf);
void VTestDriver___024root___eval_settle(VTestDriver___024root* vlSelf);
void VTestDriver___024root___eval(VTestDriver___024root* vlSelf);

void VTestDriver::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate VTestDriver::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    VTestDriver___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    vlSymsp->__Vm_deleter.deleteAll();
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        VTestDriver___024root___eval_static(&(vlSymsp->TOP));
        VTestDriver___024root___eval_initial(&(vlSymsp->TOP));
        VTestDriver___024root___eval_settle(&(vlSymsp->TOP));
    }
    // MTask 0 start
    VL_DEBUG_IF(VL_DBG_MSGF("MTask0 starting\n"););
    Verilated::mtaskId(0);
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    VTestDriver___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
    Verilated::endOfThreadMTask(vlSymsp->__Vm_evalMsgQp);
    Verilated::endOfEval(vlSymsp->__Vm_evalMsgQp);
}

//============================================================
// Events and timing
bool VTestDriver::eventsPending() { return !vlSymsp->TOP.__VdlySched.empty(); }

uint64_t VTestDriver::nextTimeSlot() { return vlSymsp->TOP.__VdlySched.nextTimeSlot(); }

//============================================================
// Utilities

const char* VTestDriver::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void VTestDriver___024root___eval_final(VTestDriver___024root* vlSelf);

VL_ATTR_COLD void VTestDriver::final() {
    VTestDriver___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* VTestDriver::hierName() const { return vlSymsp->name(); }
const char* VTestDriver::modelName() const { return "VTestDriver"; }
unsigned VTestDriver::threads() const { return 1; }
