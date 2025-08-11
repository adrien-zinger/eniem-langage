//! Interpretation of an assignation expression.
use memory;

use crate::interpreter::{job::*, *};
use exec_tree;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    /// Assign directly a value into memory and complete the Job.
    /// See `Interpreter::complete_job` in common.rs.
    fn assign_str(&self, value: &str, assign: &Assignation, job: Job) {
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign a string to {key}, {value}");
        if self.is_abstract {
            let varbox = memory::abstract_string();
            job.scope.memory.abstr_write(key, varbox);
        } else {
            let varbox = memory::string(value);
            if assign.modify {
                job.scope.memory.write_copy(key, varbox);
            } else {
                job.scope.memory.write(key, varbox);
            }
        }
        self.complete_job(job);
    }

    /// Assign directly a value into memory and complete the Job.
    /// See `Interpreter::complete_job` in common.rs.
    fn assign_num(&self, value: i32, assign: &Assignation, job: Job) {
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign a number to {key}");
        if self.is_abstract {
            let varbox = memory::abstract_number();
            job.scope.memory.abstr_write(key, varbox);
        } else {
            let varbox = memory::number(value);
            if assign.modify {
                job.scope.memory.write_copy(key, varbox);
            } else {
                job.scope.memory.write(key, varbox);
            }
        }
        self.complete_job(job);
    }

    /// Assign directly a value into memory and complete the Job.
    /// See `Interpreter::complete_job` in common.rs.
    fn assign_bool(&self, value: bool, assign: &Assignation, job: Job) {
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign a number to {key}");
        if self.is_abstract {
            let varbox = memory::abstract_boolean();
            job.scope.memory.abstr_write(key, varbox);
        } else {
            let varbox = memory::boolean(value);
            if assign.modify {
                job.scope.memory.write_copy(key, varbox);
            } else {
                job.scope.memory.write(key, varbox);
            }
        }
        self.complete_job(job);
    }

    /// Interprets function assignation or declaration.
    ///
    /// This function is called when an assignation with a function
    /// statement as right part is detected. Note that we do not
    /// consider the "value" of the right part.
    ///
    /// ```ignore
    ///    let foo = (){ ... } // function assignation
    ///    let bar = foo       // ref assignation
    ///    let foo = {(){...}} // compound assignation
    /// ```
    ///
    /// - Interpreter must push in scope memory a copy of the execution
    /// tree of targeted function.
    /// - Put an alias to each captured variable into the same memory
    /// region. The captured variable are destroyed from memory only
    /// if all occurence of that variable are destroyed (that one included).
    /// - If any of the captured variable isn't initialized, reschedule the
    /// job.
    ///
    /// # Abstract interpreter
    ///
    /// The abstract interpreter store the function in `Interpreter::functions`
    /// HashMap. Also, the write into memory has a side effect to the exec_tree
    /// itself. Look at the memory module for more information `memory::abstr_write`.
    fn assign_function(&self, input: &exec_tree::Function, assign: &Assignation, job: Job) {
        debug!("Assign a function");
        debug!("function refs: {:?}", assign.to_assign.refs);
        let mut captures = vec![];
        for c in &input.captures {
            if let Some(var) = job.scope.memory.find(c, &job) {
                captures.push((c.clone(), var));
            } else {
                self.schedule(job);
                return;
            }
        }
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("Write {:?} into {}", input, key);

        if self.is_abstract {
            if let Ok(functions) = &mut self.functions.lock() {
                if !functions.contains_key(&input.id) {
                    functions.insert(input.id.clone(), input.clone());
                }
            }
            job.scope
                .memory
                .abstr_write(key, memory::function(input.clone(), captures));
        } else if assign.modify {
            job.scope
                .memory
                .write_copy(key, memory::function(input.clone(), captures));
        } else {
            job.scope
                .memory
                .write(key, memory::function(input.clone(), captures));
        }
        self.complete_job(job);
    }

    /// Interprets a call statement. Redirect to `Interpreter::call_statement`.
    /// See module interpreter::call.
    fn assign_call(&self, input: &exec_tree::Call, assign: &Assignation, job: Job) {
        debug!("Execute Call statement in assignation");
        let id = format!("{}::{}", assign.var, job.scope.id);
        // TODO use the _ bellow
        let _ = self.call_statement(input, job, false, Some(id), assign.modify);
    }

    /// Same as call but with builtins and libc bindings. See alse `call`.
    fn assign_std_call(&self, input: &exec_tree::Call, assign: &Assignation, job: Job) {
        debug!("Execute StdCall statement in assignation");
        let id = format!("{}::{}", assign.var, job.scope.id);
        self.std_call_statement(input, job, false, Some(id), assign.modify)
    }

    /// Entry point of the interpreter assignation handling.
    ///
    /// ```ignore
    /// let a = /* any */
    /// b = /* any */
    /// ```
    ///
    /// # Inputs
    /// The input Job is generated by `Interpreter::expressions` which is
    /// called whenever the interpreter handle a Compound statement, or
    /// in the entry point of the Interpreter itself (see modules
    /// interpreter::{self, common})
    ///
    /// The `assign` variable is an inner part of the job. Already extracted
    /// because there is a good chance to enter here after a match
    /// expression to EExpression::{Assignation,Definition}.
    pub fn assignation(&self, assign: &Assignation, job: Job) {
        debug!("enter assignation, job scope id {}", job.scope.id);
        match &assign.to_assign.inner {
            EStatement::Compound(input) => self.assign_compound(input, assign, job),
            EStatement::Str(value) => self.assign_str(value, assign, job),
            EStatement::Num(value) => self.assign_num(*value, assign, job),
            EStatement::Bool(value) => self.assign_bool(*value, assign, job),
            EStatement::Function(input) => self.assign_function(input, assign, job),
            EStatement::Call(input) => self.assign_call(input, assign, job),
            EStatement::StdCall(input) => self.assign_std_call(input, assign, job),
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(input) => self.assign_ref(input, assign, job),
            EStatement::RefAs((var, ty)) => self.assign_ref_as(var, ty, assign, job),
            EStatement::Branch(_branch) => todo!(),
        }
    }
}

#[test]
fn test_assignation_ref() {
    let interpreter = Interpreter::default();
    let memory = Arc::new(Memory::default());
    let varbox = memory::string("hello world");
    memory.write("foo::1".into(), varbox);
    let scope = Arc::new(Scope {
        id: 1,
        job: None,
        len: AtomicU64::new(1),
        memory: memory.clone(),
        value: Default::default(),
    });
    let assignation = Assignation {
        block_on: false,
        var: "bar".into(),
        to_assign: Statement {
            inner: EStatement::Ref("foo".into()),
            refs: Default::default(),
        },
        modify: false,
    };
    let job = Job {
        inner: EJob::Expression(Expression {
            inner: EExpression::Assignation(assignation.clone()),
            latest: true,
        })
        .into(),
        next: None,
        fc: None,
        scope: scope.clone(),
    };
    interpreter.assignation(&assignation, job.clone());
    assert!(memory.find("bar", &job).is_some());
}

#[cfg(test)]
mod test {
    use crate::interpreter::*;
    use crate::parser::*;
    use crate::tree::{self, *};

    fn get_exec_tree(input: &str) -> Vec<exec_tree::Expression> {
        let ast: Vec<tree::Expression> = expressions(Span::new(input)).unwrap().1;
        let mut scopes = crate::scopes::Scopes::default();
        let ast = scopes.check(ast);
        assert!(scopes.errors.is_empty());
        let mut adapter = exec_tree::Scope2ETree::default();
        ast.into_iter().map(|s| adapter.expression(s)).collect()
    }

    #[test]
    fn integration_assign() {
        let tree = get_exec_tree("let a = \"hello world\"");
        let interpreter = Interpreter::default();
        let mem = interpreter.run(&tree);
        println!("{:?}", mem.map.read().unwrap());
        let a = mem.get("main#1:1:a::0").unwrap();
        assert_eq!(memory::get_types(&a), vec!["string".to_string()]);
    }

    #[test]
    #[ntest::timeout(300)]
    #[should_panic]
    fn integration_abstract_assign_cast_fail() {
        let tree = get_exec_tree(
            "
			let bar = (var) {
				i32_not_equal!(var, 0) /* wrong type detected */
			};
			let hello = \"hello world\";
			let foo = hello as bar;
		",
        );
        let interpreter = Interpreter::abstr();
        interpreter.run(&tree);
    }

    #[test]
    #[ntest::timeout(300)]
    fn integration_assign_cast() {
        let tree = get_exec_tree(
            "
			let bar = () {
				true /* the bar type can ALWAYS be applied */
			};
			let hello = \"hello world\";
			let foo = hello as bar;
		",
        );
        let interpreter = Interpreter::default();
        let mem = interpreter.run(&tree);
        println!("{:#?}", mem.map.read().unwrap());
        let b = mem.get("main#6:4:foo::0").unwrap();
        assert_eq!(
            memory::get_types(&b),
            vec!["string".to_string(), "main#2:4:bar".to_string()]
        );
    }

    #[test]
    #[ntest::timeout(300)]
    fn integration_assign_cast_not0() {
        let tree = get_exec_tree(
            "
			let not0 = (var) {
				i32_not_equal!(var, 0)
			};
			let num = 1;
			let numnot = num as not0;
		",
        );
        let interpreter = Interpreter::default();
        let mem = interpreter.run(&tree);
        println!("{:#?}", mem.map.read().unwrap());
        let b = mem.get("main#6:4:numnot::0").unwrap();
        assert_eq!(
            memory::get_types(&b),
            vec!["number".to_string(), "main#2:4:not0".to_string()]
        );
    }

    #[test]
    #[ntest::timeout(300)]
    #[should_panic]
    fn integration_assign_cast_not0_fail() {
        let tree = get_exec_tree(
            "
			let not0 = (var) {
				i32_not_equal!(var, 0)
			};
			let num = 0;
			let numnot = num as not0;
		",
        );
        let interpreter = Interpreter::default();
        let mem = interpreter.run(&tree);
        println!("{:#?}", mem.map.read().unwrap());
        let b = mem.get("main#6:4:numnot::0").unwrap();
        assert_eq!(
            memory::get_types(&b),
            vec!["number".to_string(), "main#2:4:not0".to_string()]
        );
    }

    #[test]
    #[ntest::timeout(300)]
    #[should_panic]
    fn integration_assign_cast_fail() {
        let tree = get_exec_tree(
            "
			let bar = () {
				false /* the bar type can NEVER be applied */
			};
			let hello = \"hello world\";
			let foo = hello as bar;      /* panic here */
		",
        );
        let interpreter = Interpreter::default();
        let mem = interpreter.run(&tree);
    }
}
