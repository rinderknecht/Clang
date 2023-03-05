using System.Collections.Generic;
using Cortus.NanoSharp.Bytecode.Mechanisms;
using Cortus.NanoSharp.Cecil;
using Cortus.NanoSharp.Metamodel;

namespace Cortus.NanoSharp.TypeAnalysis
{
    /// <summary>Implementation of the Rapid Type Analysis.</summary>
    /// <content>This file contains the main method of the algorithm.</content>
    public partial class RapidTypeAnalysis {
//        private Program _program;

        private bool _processed;
        private readonly Queue<LocalMethod> _queue;
        public readonly Dictionary<ILateBinding, LateBindingSite> CallSites;

        public readonly List<INew> Instantiations;
        public readonly List<INullCheck> NullChecks;
        public readonly List<ISubtypeTest> SubtypeTests;
        public readonly List<IArrayStoreCheck> ArrayStoreChecks;
        public readonly List<IFieldAccess> FieldAccesses;
        public readonly List<IMethodCall> MethodCalls;
        public readonly List<IMethodFunctionPointer> MethodFunctionPointers;

        public LocalMethod EntryPoint { get; private set; }
        private readonly HashSet<IFormalType> _recursiveTypes;

        public RapidTypeAnalysis()
        {
            _processed = false;
            _queue = new Queue<LocalMethod>();
            CallSites = new Dictionary<ILateBinding, LateBindingSite>();

            Instantiations = new List<INew>();
            NullChecks = new List<INullCheck>();
            SubtypeTests = new List<ISubtypeTest>();
            ArrayStoreChecks = new List<IArrayStoreCheck>();
            FieldAccesses = new List<IFieldAccess>();
            MethodCalls = new List<IMethodCall>();
            MethodFunctionPointers = new List<IMethodFunctionPointer>();

            _recursiveTypes = new HashSet<IFormalType>();
        }

        public void ProcessProgram(Program program)
        {
//            _program = program;

            this.LoadCoreClasses();
            this.LoadCoreExceptions();

            Compiler.ClassLoader.LoadClass(program.EntryPoint.DeclaringType);
            var entryPoint = Compiler.ClassLoader.GetMethod(program.EntryPoint);

            entryPoint.IsAlive = true;
            _queue.Enqueue(entryPoint);

            while (_queue.Count != 0) {
                // Handle instructions that implement mechanisms like new(), call, nullchecks…
                this.ProcessMethod(_queue.Dequeue());
            }

            EntryPoint = entryPoint;

            ////this.FinishClassLoading(); // TODO: This line doesn't seem useful, commenting it out doesn't change anything.
            this.ResolveGenerics(); // TODO: What does this actually do? What is the "influence graph" ?
            ResolveConcreteArrays.Resolve();

            _processed = true;

            this.MarkSubtypeTestTargets();
        }

        public bool SetMethodAlive(LocalMethod method) {
            if (method.IsAlive) {
                return false;
            } else {
                method.IsAlive = true;
                _queue.Enqueue(method);
                return true;
            }
        }

        public void SetMethodsAlive(IEnumerable<LocalMethod> methods) {
            foreach (var m in methods) {
                SetMethodAlive(m);
            }
        }
    }
}
