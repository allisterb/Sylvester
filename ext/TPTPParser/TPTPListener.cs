using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Text;

namespace TPTPParser
{
    public class TPTPListener : tptp_v7_0_0_0BaseListener
    {
        public override void EnterAnnotated_formula([NotNull] tptp_v7_0_0_0Parser.Annotated_formulaContext context)
        {
            base.EnterAnnotated_formula(context);
        }

        public override void EnterAnnotations([NotNull] tptp_v7_0_0_0Parser.AnnotationsContext context)
        {
            base.EnterAnnotations(context);
        }

        public override void EnterAssoc_connective([NotNull] tptp_v7_0_0_0Parser.Assoc_connectiveContext context)
        {
            base.EnterAssoc_connective(context);
        }

        public override void EnterAssumptions_record([NotNull] tptp_v7_0_0_0Parser.Assumptions_recordContext context)
        {
            base.EnterAssumptions_record(context);
        }

        public override void EnterAtom([NotNull] tptp_v7_0_0_0Parser.AtomContext context)
        {
            base.EnterAtom(context);
        }

        public override void EnterAtomic_defined_word([NotNull] tptp_v7_0_0_0Parser.Atomic_defined_wordContext context)
        {
            base.EnterAtomic_defined_word(context);
        }

        public override void EnterAtomic_system_word([NotNull] tptp_v7_0_0_0Parser.Atomic_system_wordContext context)
        {
            base.EnterAtomic_system_word(context);
        }

        public override void EnterAtomic_word([NotNull] tptp_v7_0_0_0Parser.Atomic_wordContext context)
        {
            base.EnterAtomic_word(context);
        }

        public override void EnterBinary_connective([NotNull] tptp_v7_0_0_0Parser.Binary_connectiveContext context)
        {
            base.EnterBinary_connective(context);
        }

        public override void EnterCnf_annotated([NotNull] tptp_v7_0_0_0Parser.Cnf_annotatedContext context)
        {
            base.EnterCnf_annotated(context);
        }

        public override void EnterCnf_disjunction([NotNull] tptp_v7_0_0_0Parser.Cnf_disjunctionContext context)
        {
            base.EnterCnf_disjunction(context);
        }

        public override void EnterCnf_formula([NotNull] tptp_v7_0_0_0Parser.Cnf_formulaContext context)
        {
            base.EnterCnf_formula(context);
        }

        public override void EnterCnf_literal([NotNull] tptp_v7_0_0_0Parser.Cnf_literalContext context)
        {
            base.EnterCnf_literal(context);
        }

        public override void EnterConstant([NotNull] tptp_v7_0_0_0Parser.ConstantContext context)
        {
            base.EnterConstant(context);
        }

        public override void EnterCreator_name([NotNull] tptp_v7_0_0_0Parser.Creator_nameContext context)
        {
            base.EnterCreator_name(context);
        }

        public override void EnterCreator_source([NotNull] tptp_v7_0_0_0Parser.Creator_sourceContext context)
        {
            base.EnterCreator_source(context);
        }

        public override void EnterDag_source([NotNull] tptp_v7_0_0_0Parser.Dag_sourceContext context)
        {
            base.EnterDag_source(context);
        }

        public override void EnterDefined_constant([NotNull] tptp_v7_0_0_0Parser.Defined_constantContext context)
        {
            base.EnterDefined_constant(context);
        }

        public override void EnterDefined_functor([NotNull] tptp_v7_0_0_0Parser.Defined_functorContext context)
        {
            base.EnterDefined_functor(context);
        }

        public override void EnterDefined_infix_pred([NotNull] tptp_v7_0_0_0Parser.Defined_infix_predContext context)
        {
            base.EnterDefined_infix_pred(context);
        }

        public override void EnterDefined_predicate([NotNull] tptp_v7_0_0_0Parser.Defined_predicateContext context)
        {
            base.EnterDefined_predicate(context);
        }

        public override void EnterDefined_proposition([NotNull] tptp_v7_0_0_0Parser.Defined_propositionContext context)
        {
            base.EnterDefined_proposition(context);
        }

        public override void EnterDefined_term([NotNull] tptp_v7_0_0_0Parser.Defined_termContext context)
        {
            base.EnterDefined_term(context);
        }

        public override void EnterDefined_type([NotNull] tptp_v7_0_0_0Parser.Defined_typeContext context)
        {
            base.EnterDefined_type(context);
        }

        public override void EnterDescription_item([NotNull] tptp_v7_0_0_0Parser.Description_itemContext context)
        {
            base.EnterDescription_item(context);
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
            base.EnterEveryRule(context);
        }

        public override void EnterExternal_source([NotNull] tptp_v7_0_0_0Parser.External_sourceContext context)
        {
            base.EnterExternal_source(context);
        }

        public override void EnterFile_info([NotNull] tptp_v7_0_0_0Parser.File_infoContext context)
        {
            base.EnterFile_info(context);
        }

        public override void EnterFile_name([NotNull] tptp_v7_0_0_0Parser.File_nameContext context)
        {
            base.EnterFile_name(context);
        }

        public override void EnterFile_source([NotNull] tptp_v7_0_0_0Parser.File_sourceContext context)
        {
            base.EnterFile_source(context);
        }

        public override void EnterFof_and_formula([NotNull] tptp_v7_0_0_0Parser.Fof_and_formulaContext context)
        {
            base.EnterFof_and_formula(context);
        }

        public override void EnterFof_annotated([NotNull] tptp_v7_0_0_0Parser.Fof_annotatedContext context)
        {
            base.EnterFof_annotated(context);
        }

        public override void EnterFof_arguments([NotNull] tptp_v7_0_0_0Parser.Fof_argumentsContext context)
        {
            base.EnterFof_arguments(context);
        }

        public override void EnterFof_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_atomic_formulaContext context)
        {
            base.EnterFof_atomic_formula(context);
        }

        public override void EnterFof_binary_assoc([NotNull] tptp_v7_0_0_0Parser.Fof_binary_assocContext context)
        {
            base.EnterFof_binary_assoc(context);
        }

        public override void EnterFof_binary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_binary_formulaContext context)
        {
            base.EnterFof_binary_formula(context);
        }

        public override void EnterFof_binary_nonassoc([NotNull] tptp_v7_0_0_0Parser.Fof_binary_nonassocContext context)
        {
            base.EnterFof_binary_nonassoc(context);
        }

        public override void EnterFof_defined_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_atomic_formulaContext context)
        {
            base.EnterFof_defined_atomic_formula(context);
        }

        public override void EnterFof_defined_atomic_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_atomic_termContext context)
        {
            base.EnterFof_defined_atomic_term(context);
        }

        public override void EnterFof_defined_infix_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_infix_formulaContext context)
        {
            base.EnterFof_defined_infix_formula(context);
        }

        public override void EnterFof_defined_plain_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_plain_formulaContext context)
        {
            base.EnterFof_defined_plain_formula(context);
        }

        public override void EnterFof_defined_plain_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_plain_termContext context)
        {
            base.EnterFof_defined_plain_term(context);
        }

        public override void EnterFof_defined_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_termContext context)
        {
            base.EnterFof_defined_term(context);
        }

        public override void EnterFof_formula([NotNull] tptp_v7_0_0_0Parser.Fof_formulaContext context)
        {
            base.EnterFof_formula(context);
        }

        public override void EnterFof_formula_tuple([NotNull] tptp_v7_0_0_0Parser.Fof_formula_tupleContext context)
        {
            base.EnterFof_formula_tuple(context);
        }

        public override void EnterFof_formula_tuple_list([NotNull] tptp_v7_0_0_0Parser.Fof_formula_tuple_listContext context)
        {
            base.EnterFof_formula_tuple_list(context);
        }

        public override void EnterFof_function_term([NotNull] tptp_v7_0_0_0Parser.Fof_function_termContext context)
        {
            base.EnterFof_function_term(context);
        }

        public override void EnterFof_infix_unary([NotNull] tptp_v7_0_0_0Parser.Fof_infix_unaryContext context)
        {
            base.EnterFof_infix_unary(context);
        }

        public override void EnterFof_logic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_logic_formulaContext context)
        {
            base.EnterFof_logic_formula(context);
        }

        public override void EnterFof_or_formula([NotNull] tptp_v7_0_0_0Parser.Fof_or_formulaContext context)
        {
            base.EnterFof_or_formula(context);
        }

        public override void EnterFof_plain_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_plain_atomic_formulaContext context)
        {
            base.EnterFof_plain_atomic_formula(context);
        }

        public override void EnterFof_plain_term([NotNull] tptp_v7_0_0_0Parser.Fof_plain_termContext context)
        {
            base.EnterFof_plain_term(context);
        }

        public override void EnterFof_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Fof_quantified_formulaContext context)
        {
            base.EnterFof_quantified_formula(context);
        }

        public override void EnterFof_quantifier([NotNull] tptp_v7_0_0_0Parser.Fof_quantifierContext context)
        {
            base.EnterFof_quantifier(context);
        }

        public override void EnterFof_sequent([NotNull] tptp_v7_0_0_0Parser.Fof_sequentContext context)
        {
            base.EnterFof_sequent(context);
        }

        public override void EnterFof_system_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_system_atomic_formulaContext context)
        {
            base.EnterFof_system_atomic_formula(context);
        }

        public override void EnterFof_system_term([NotNull] tptp_v7_0_0_0Parser.Fof_system_termContext context)
        {
            base.EnterFof_system_term(context);
        }

        public override void EnterFof_term([NotNull] tptp_v7_0_0_0Parser.Fof_termContext context)
        {
            base.EnterFof_term(context);
        }

        public override void EnterFof_unary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_unary_formulaContext context)
        {
            base.EnterFof_unary_formula(context);
        }

        public override void EnterFof_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_unitary_formulaContext context)
        {
            base.EnterFof_unitary_formula(context);
        }

        public override void EnterFof_variable_list([NotNull] tptp_v7_0_0_0Parser.Fof_variable_listContext context)
        {
            base.EnterFof_variable_list(context);
        }

        public override void EnterFormula_data([NotNull] tptp_v7_0_0_0Parser.Formula_dataContext context)
        {
            base.EnterFormula_data(context);
        }

        public override void EnterFormula_item([NotNull] tptp_v7_0_0_0Parser.Formula_itemContext context)
        {
            base.EnterFormula_item(context);
        }

        public override void EnterFormula_role([NotNull] tptp_v7_0_0_0Parser.Formula_roleContext context)
        {
            base.EnterFormula_role(context);
        }

        public override void EnterFormula_selection([NotNull] tptp_v7_0_0_0Parser.Formula_selectionContext context)
        {
            base.EnterFormula_selection(context);
        }

        public override void EnterFunctor([NotNull] tptp_v7_0_0_0Parser.FunctorContext context)
        {
            base.EnterFunctor(context);
        }

        public override void EnterGeneral_data([NotNull] tptp_v7_0_0_0Parser.General_dataContext context)
        {
            base.EnterGeneral_data(context);
        }

        public override void EnterGeneral_function([NotNull] tptp_v7_0_0_0Parser.General_functionContext context)
        {
            base.EnterGeneral_function(context);
        }

        public override void EnterGeneral_list([NotNull] tptp_v7_0_0_0Parser.General_listContext context)
        {
            base.EnterGeneral_list(context);
        }

        public override void EnterGeneral_term([NotNull] tptp_v7_0_0_0Parser.General_termContext context)
        {
            base.EnterGeneral_term(context);
        }

        public override void EnterGeneral_terms([NotNull] tptp_v7_0_0_0Parser.General_termsContext context)
        {
            base.EnterGeneral_terms(context);
        }

        public override void EnterInclude([NotNull] tptp_v7_0_0_0Parser.IncludeContext context)
        {
            base.EnterInclude(context);
        }

        public override void EnterInference_info([NotNull] tptp_v7_0_0_0Parser.Inference_infoContext context)
        {
            base.EnterInference_info(context);
        }

        public override void EnterInference_item([NotNull] tptp_v7_0_0_0Parser.Inference_itemContext context)
        {
            base.EnterInference_item(context);
        }

        public override void EnterInference_parents([NotNull] tptp_v7_0_0_0Parser.Inference_parentsContext context)
        {
            base.EnterInference_parents(context);
        }

        public override void EnterInference_record([NotNull] tptp_v7_0_0_0Parser.Inference_recordContext context)
        {
            base.EnterInference_record(context);
        }

        public override void EnterInference_rule([NotNull] tptp_v7_0_0_0Parser.Inference_ruleContext context)
        {
            base.EnterInference_rule(context);
        }

        public override void EnterInference_status([NotNull] tptp_v7_0_0_0Parser.Inference_statusContext context)
        {
            base.EnterInference_status(context);
        }

        public override void EnterInfo_item([NotNull] tptp_v7_0_0_0Parser.Info_itemContext context)
        {
            base.EnterInfo_item(context);
        }

        public override void EnterInfo_items([NotNull] tptp_v7_0_0_0Parser.Info_itemsContext context)
        {
            base.EnterInfo_items(context);
        }

        public override void EnterInternal_source([NotNull] tptp_v7_0_0_0Parser.Internal_sourceContext context)
        {
            base.EnterInternal_source(context);
        }

        public override void EnterIntro_type([NotNull] tptp_v7_0_0_0Parser.Intro_typeContext context)
        {
            base.EnterIntro_type(context);
        }

        public override void EnterIquote_item([NotNull] tptp_v7_0_0_0Parser.Iquote_itemContext context)
        {
            base.EnterIquote_item(context);
        }

        public override void EnterName([NotNull] tptp_v7_0_0_0Parser.NameContext context)
        {
            base.EnterName(context);
        }

        public override void EnterName_list([NotNull] tptp_v7_0_0_0Parser.Name_listContext context)
        {
            base.EnterName_list(context);
        }

        public override void EnterNew_symbol_list([NotNull] tptp_v7_0_0_0Parser.New_symbol_listContext context)
        {
            base.EnterNew_symbol_list(context);
        }

        public override void EnterNew_symbol_record([NotNull] tptp_v7_0_0_0Parser.New_symbol_recordContext context)
        {
            base.EnterNew_symbol_record(context);
        }

        public override void EnterNumber([NotNull] tptp_v7_0_0_0Parser.NumberContext context)
        {
            base.EnterNumber(context);
        }

        public override void EnterOptional_info([NotNull] tptp_v7_0_0_0Parser.Optional_infoContext context)
        {
            base.EnterOptional_info(context);
        }

        public override void EnterParent_details([NotNull] tptp_v7_0_0_0Parser.Parent_detailsContext context)
        {
            base.EnterParent_details(context);
        }

        public override void EnterParent_info([NotNull] tptp_v7_0_0_0Parser.Parent_infoContext context)
        {
            base.EnterParent_info(context);
        }

        public override void EnterParent_list([NotNull] tptp_v7_0_0_0Parser.Parent_listContext context)
        {
            base.EnterParent_list(context);
        }

        public override void EnterPrincipal_symbol([NotNull] tptp_v7_0_0_0Parser.Principal_symbolContext context)
        {
            base.EnterPrincipal_symbol(context);
        }

        public override void EnterRefutation([NotNull] tptp_v7_0_0_0Parser.RefutationContext context)
        {
            base.EnterRefutation(context);
        }

        public override void EnterSource([NotNull] tptp_v7_0_0_0Parser.SourceContext context)
        {
            base.EnterSource(context);
        }

        public override void EnterSources([NotNull] tptp_v7_0_0_0Parser.SourcesContext context)
        {
            base.EnterSources(context);
        }

        public override void EnterStatus_value([NotNull] tptp_v7_0_0_0Parser.Status_valueContext context)
        {
            base.EnterStatus_value(context);
        }

        public override void EnterSystem_constant([NotNull] tptp_v7_0_0_0Parser.System_constantContext context)
        {
            base.EnterSystem_constant(context);
        }

        public override void EnterSystem_functor([NotNull] tptp_v7_0_0_0Parser.System_functorContext context)
        {
            base.EnterSystem_functor(context);
        }

        public override void EnterSystem_type([NotNull] tptp_v7_0_0_0Parser.System_typeContext context)
        {
            base.EnterSystem_type(context);
        }

        public override void EnterTcf_annotated([NotNull] tptp_v7_0_0_0Parser.Tcf_annotatedContext context)
        {
            base.EnterTcf_annotated(context);
        }

        public override void EnterTcf_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_formulaContext context)
        {
            base.EnterTcf_formula(context);
        }

        public override void EnterTcf_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_logic_formulaContext context)
        {
            base.EnterTcf_logic_formula(context);
        }

        public override void EnterTcf_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_quantified_formulaContext context)
        {
            base.EnterTcf_quantified_formula(context);
        }

        public override void EnterTf1_quantified_type([NotNull] tptp_v7_0_0_0Parser.Tf1_quantified_typeContext context)
        {
            base.EnterTf1_quantified_type(context);
        }

        public override void EnterTff_and_formula([NotNull] tptp_v7_0_0_0Parser.Tff_and_formulaContext context)
        {
            base.EnterTff_and_formula(context);
        }

        public override void EnterTff_annotated([NotNull] tptp_v7_0_0_0Parser.Tff_annotatedContext context)
        {
            base.EnterTff_annotated(context);
        }

        public override void EnterTff_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Tff_atomic_formulaContext context)
        {
            base.EnterTff_atomic_formula(context);
        }

        public override void EnterTff_atomic_type([NotNull] tptp_v7_0_0_0Parser.Tff_atomic_typeContext context)
        {
            base.EnterTff_atomic_type(context);
        }

        public override void EnterTff_binary_assoc([NotNull] tptp_v7_0_0_0Parser.Tff_binary_assocContext context)
        {
            base.EnterTff_binary_assoc(context);
        }

        public override void EnterTff_binary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_binary_formulaContext context)
        {
            base.EnterTff_binary_formula(context);
        }

        public override void EnterTff_binary_nonassoc([NotNull] tptp_v7_0_0_0Parser.Tff_binary_nonassocContext context)
        {
            base.EnterTff_binary_nonassoc(context);
        }

        public override void EnterTff_conditional([NotNull] tptp_v7_0_0_0Parser.Tff_conditionalContext context)
        {
            base.EnterTff_conditional(context);
        }

        public override void EnterTff_conditional_term([NotNull] tptp_v7_0_0_0Parser.Tff_conditional_termContext context)
        {
            base.EnterTff_conditional_term(context);
        }

        public override void EnterTff_formula([NotNull] tptp_v7_0_0_0Parser.Tff_formulaContext context)
        {
            base.EnterTff_formula(context);
        }

        public override void EnterTff_formula_tuple([NotNull] tptp_v7_0_0_0Parser.Tff_formula_tupleContext context)
        {
            base.EnterTff_formula_tuple(context);
        }

        public override void EnterTff_formula_tuple_list([NotNull] tptp_v7_0_0_0Parser.Tff_formula_tuple_listContext context)
        {
            base.EnterTff_formula_tuple_list(context);
        }

        public override void EnterTff_let([NotNull] tptp_v7_0_0_0Parser.Tff_letContext context)
        {
            base.EnterTff_let(context);
        }

        public override void EnterTff_let_formula_binding([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_bindingContext context)
        {
            base.EnterTff_let_formula_binding(context);
        }

        public override void EnterTff_let_formula_defn([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_defnContext context)
        {
            base.EnterTff_let_formula_defn(context);
        }

        public override void EnterTff_let_formula_defns([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_defnsContext context)
        {
            base.EnterTff_let_formula_defns(context);
        }

        public override void EnterTff_let_formula_list([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_listContext context)
        {
            base.EnterTff_let_formula_list(context);
        }

        public override void EnterTff_let_term([NotNull] tptp_v7_0_0_0Parser.Tff_let_termContext context)
        {
            base.EnterTff_let_term(context);
        }

        public override void EnterTff_let_term_binding([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_bindingContext context)
        {
            base.EnterTff_let_term_binding(context);
        }

        public override void EnterTff_let_term_defn([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_defnContext context)
        {
            base.EnterTff_let_term_defn(context);
        }

        public override void EnterTff_let_term_defns([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_defnsContext context)
        {
            base.EnterTff_let_term_defns(context);
        }

        public override void EnterTff_let_term_list([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_listContext context)
        {
            base.EnterTff_let_term_list(context);
        }

        public override void EnterTff_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tff_logic_formulaContext context)
        {
            base.EnterTff_logic_formula(context);
        }

        public override void EnterTff_mapping_type([NotNull] tptp_v7_0_0_0Parser.Tff_mapping_typeContext context)
        {
            base.EnterTff_mapping_type(context);
        }

        public override void EnterTff_monotype([NotNull] tptp_v7_0_0_0Parser.Tff_monotypeContext context)
        {
            base.EnterTff_monotype(context);
        }

        public override void EnterTff_or_formula([NotNull] tptp_v7_0_0_0Parser.Tff_or_formulaContext context)
        {
            base.EnterTff_or_formula(context);
        }

        public override void EnterTff_pair_connective([NotNull] tptp_v7_0_0_0Parser.Tff_pair_connectiveContext context)
        {
            base.EnterTff_pair_connective(context);
        }

        public override void EnterTff_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Tff_quantified_formulaContext context)
        {
            base.EnterTff_quantified_formula(context);
        }

        public override void EnterTff_sequent([NotNull] tptp_v7_0_0_0Parser.Tff_sequentContext context)
        {
            base.EnterTff_sequent(context);
        }

        public override void EnterTff_subtype([NotNull] tptp_v7_0_0_0Parser.Tff_subtypeContext context)
        {
            base.EnterTff_subtype(context);
        }

        public override void EnterTff_top_level_type([NotNull] tptp_v7_0_0_0Parser.Tff_top_level_typeContext context)
        {
            base.EnterTff_top_level_type(context);
        }

        public override void EnterTff_tuple_term([NotNull] tptp_v7_0_0_0Parser.Tff_tuple_termContext context)
        {
            base.EnterTff_tuple_term(context);
        }

        public override void EnterTff_typed_atom([NotNull] tptp_v7_0_0_0Parser.Tff_typed_atomContext context)
        {
            base.EnterTff_typed_atom(context);
        }

        public override void EnterTff_typed_variable([NotNull] tptp_v7_0_0_0Parser.Tff_typed_variableContext context)
        {
            base.EnterTff_typed_variable(context);
        }

        public override void EnterTff_type_arguments([NotNull] tptp_v7_0_0_0Parser.Tff_type_argumentsContext context)
        {
            base.EnterTff_type_arguments(context);
        }

        public override void EnterTff_unary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_unary_formulaContext context)
        {
            base.EnterTff_unary_formula(context);
        }

        public override void EnterTff_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_unitary_formulaContext context)
        {
            base.EnterTff_unitary_formula(context);
        }

        public override void EnterTff_unitary_type([NotNull] tptp_v7_0_0_0Parser.Tff_unitary_typeContext context)
        {
            base.EnterTff_unitary_type(context);
        }

        public override void EnterTff_variable([NotNull] tptp_v7_0_0_0Parser.Tff_variableContext context)
        {
            base.EnterTff_variable(context);
        }

        public override void EnterTff_variable_list([NotNull] tptp_v7_0_0_0Parser.Tff_variable_listContext context)
        {
            base.EnterTff_variable_list(context);
        }

        public override void EnterTff_xprod_type([NotNull] tptp_v7_0_0_0Parser.Tff_xprod_typeContext context)
        {
            base.EnterTff_xprod_type(context);
        }

        public override void EnterTfx_annotated([NotNull] tptp_v7_0_0_0Parser.Tfx_annotatedContext context)
        {
            base.EnterTfx_annotated(context);
        }

        public override void EnterTfx_formula([NotNull] tptp_v7_0_0_0Parser.Tfx_formulaContext context)
        {
            base.EnterTfx_formula(context);
        }

        public override void EnterTfx_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tfx_logic_formulaContext context)
        {
            base.EnterTfx_logic_formula(context);
        }

        public override void EnterTh0_quantifier([NotNull] tptp_v7_0_0_0Parser.Th0_quantifierContext context)
        {
            base.EnterTh0_quantifier(context);
        }

        public override void EnterTh1_quantifier([NotNull] tptp_v7_0_0_0Parser.Th1_quantifierContext context)
        {
            base.EnterTh1_quantifier(context);
        }

        public override void EnterTh1_unary_connective([NotNull] tptp_v7_0_0_0Parser.Th1_unary_connectiveContext context)
        {
            base.EnterTh1_unary_connective(context);
        }

        public override void EnterTheory([NotNull] tptp_v7_0_0_0Parser.TheoryContext context)
        {
            base.EnterTheory(context);
        }

        public override void EnterTheory_name([NotNull] tptp_v7_0_0_0Parser.Theory_nameContext context)
        {
            base.EnterTheory_name(context);
        }

        public override void EnterThf_and_formula([NotNull] tptp_v7_0_0_0Parser.Thf_and_formulaContext context)
        {
            base.EnterThf_and_formula(context);
        }

        public override void EnterThf_annotated([NotNull] tptp_v7_0_0_0Parser.Thf_annotatedContext context)
        {
            base.EnterThf_annotated(context);
        }

        public override void EnterThf_apply_formula([NotNull] tptp_v7_0_0_0Parser.Thf_apply_formulaContext context)
        {
            base.EnterThf_apply_formula(context);
        }

        public override void EnterThf_apply_type([NotNull] tptp_v7_0_0_0Parser.Thf_apply_typeContext context)
        {
            base.EnterThf_apply_type(context);
        }

        public override void EnterThf_arguments([NotNull] tptp_v7_0_0_0Parser.Thf_argumentsContext context)
        {
            base.EnterThf_arguments(context);
        }

        public override void EnterThf_atom([NotNull] tptp_v7_0_0_0Parser.Thf_atomContext context)
        {
            base.EnterThf_atom(context);
        }

        public override void EnterThf_binary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_binary_formulaContext context)
        {
            base.EnterThf_binary_formula(context);
        }

        public override void EnterThf_binary_pair([NotNull] tptp_v7_0_0_0Parser.Thf_binary_pairContext context)
        {
            base.EnterThf_binary_pair(context);
        }

        public override void EnterThf_binary_tuple([NotNull] tptp_v7_0_0_0Parser.Thf_binary_tupleContext context)
        {
            base.EnterThf_binary_tuple(context);
        }

        public override void EnterThf_binary_type([NotNull] tptp_v7_0_0_0Parser.Thf_binary_typeContext context)
        {
            base.EnterThf_binary_type(context);
        }

        public override void EnterThf_conditional([NotNull] tptp_v7_0_0_0Parser.Thf_conditionalContext context)
        {
            base.EnterThf_conditional(context);
        }

        public override void EnterThf_conn_term([NotNull] tptp_v7_0_0_0Parser.Thf_conn_termContext context)
        {
            base.EnterThf_conn_term(context);
        }

        public override void EnterThf_formula([NotNull] tptp_v7_0_0_0Parser.Thf_formulaContext context)
        {
            base.EnterThf_formula(context);
        }

        public override void EnterThf_formula_list([NotNull] tptp_v7_0_0_0Parser.Thf_formula_listContext context)
        {
            base.EnterThf_formula_list(context);
        }

        public override void EnterThf_function([NotNull] tptp_v7_0_0_0Parser.Thf_functionContext context)
        {
            base.EnterThf_function(context);
        }

        public override void EnterThf_let([NotNull] tptp_v7_0_0_0Parser.Thf_letContext context)
        {
            base.EnterThf_let(context);
        }

        public override void EnterThf_logic_formula([NotNull] tptp_v7_0_0_0Parser.Thf_logic_formulaContext context)
        {
            base.EnterThf_logic_formula(context);
        }

        public override void EnterThf_mapping_type([NotNull] tptp_v7_0_0_0Parser.Thf_mapping_typeContext context)
        {
            base.EnterThf_mapping_type(context);
        }

        public override void EnterThf_or_formula([NotNull] tptp_v7_0_0_0Parser.Thf_or_formulaContext context)
        {
            base.EnterThf_or_formula(context);
        }

        public override void EnterThf_pair_connective([NotNull] tptp_v7_0_0_0Parser.Thf_pair_connectiveContext context)
        {
            base.EnterThf_pair_connective(context);
        }

        public override void EnterThf_quantification([NotNull] tptp_v7_0_0_0Parser.Thf_quantificationContext context)
        {
            base.EnterThf_quantification(context);
        }

        public override void EnterThf_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Thf_quantified_formulaContext context)
        {
            base.EnterThf_quantified_formula(context);
        }

        public override void EnterThf_quantifier([NotNull] tptp_v7_0_0_0Parser.Thf_quantifierContext context)
        {
            base.EnterThf_quantifier(context);
        }

        public override void EnterThf_sequent([NotNull] tptp_v7_0_0_0Parser.Thf_sequentContext context)
        {
            base.EnterThf_sequent(context);
        }

        public override void EnterThf_subtype([NotNull] tptp_v7_0_0_0Parser.Thf_subtypeContext context)
        {
            base.EnterThf_subtype(context);
        }

        public override void EnterThf_top_level_type([NotNull] tptp_v7_0_0_0Parser.Thf_top_level_typeContext context)
        {
            base.EnterThf_top_level_type(context);
        }

        public override void EnterThf_tuple([NotNull] tptp_v7_0_0_0Parser.Thf_tupleContext context)
        {
            base.EnterThf_tuple(context);
        }

        public override void EnterThf_typeable_formula([NotNull] tptp_v7_0_0_0Parser.Thf_typeable_formulaContext context)
        {
            base.EnterThf_typeable_formula(context);
        }

        public override void EnterThf_typed_variable([NotNull] tptp_v7_0_0_0Parser.Thf_typed_variableContext context)
        {
            base.EnterThf_typed_variable(context);
        }

        public override void EnterThf_type_formula([NotNull] tptp_v7_0_0_0Parser.Thf_type_formulaContext context)
        {
            base.EnterThf_type_formula(context);
        }

        public override void EnterThf_unary_connective([NotNull] tptp_v7_0_0_0Parser.Thf_unary_connectiveContext context)
        {
            base.EnterThf_unary_connective(context);
        }

        public override void EnterThf_unary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_unary_formulaContext context)
        {
            base.EnterThf_unary_formula(context);
        }

        public override void EnterThf_union_type([NotNull] tptp_v7_0_0_0Parser.Thf_union_typeContext context)
        {
            base.EnterThf_union_type(context);
        }

        public override void EnterThf_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_unitary_formulaContext context)
        {
            base.EnterThf_unitary_formula(context);
        }

        public override void EnterThf_unitary_type([NotNull] tptp_v7_0_0_0Parser.Thf_unitary_typeContext context)
        {
            base.EnterThf_unitary_type(context);
        }

        public override void EnterThf_variable([NotNull] tptp_v7_0_0_0Parser.Thf_variableContext context)
        {
            base.EnterThf_variable(context);
        }

        public override void EnterThf_xprod_type([NotNull] tptp_v7_0_0_0Parser.Thf_xprod_typeContext context)
        {
            base.EnterThf_xprod_type(context);
        }

        public override void EnterTpi_annotated([NotNull] tptp_v7_0_0_0Parser.Tpi_annotatedContext context)
        {
            base.EnterTpi_annotated(context);
        }

        public override void EnterTpi_formula([NotNull] tptp_v7_0_0_0Parser.Tpi_formulaContext context)
        {
            base.EnterTpi_formula(context);
        }

        public override void EnterTptp_file([NotNull] tptp_v7_0_0_0Parser.Tptp_fileContext context)
        {
            base.EnterTptp_file(context);
        }

        public override void EnterTptp_input([NotNull] tptp_v7_0_0_0Parser.Tptp_inputContext context)
        {
            base.EnterTptp_input(context);
        }

        public override void EnterType_constant([NotNull] tptp_v7_0_0_0Parser.Type_constantContext context)
        {
            base.EnterType_constant(context);
        }

        public override void EnterType_functor([NotNull] tptp_v7_0_0_0Parser.Type_functorContext context)
        {
            base.EnterType_functor(context);
        }

        public override void EnterUnary_connective([NotNull] tptp_v7_0_0_0Parser.Unary_connectiveContext context)
        {
            base.EnterUnary_connective(context);
        }

        public override void EnterUntyped_atom([NotNull] tptp_v7_0_0_0Parser.Untyped_atomContext context)
        {
            base.EnterUntyped_atom(context);
        }

        public override void EnterUseful_info([NotNull] tptp_v7_0_0_0Parser.Useful_infoContext context)
        {
            base.EnterUseful_info(context);
        }

        public override void EnterVariable([NotNull] tptp_v7_0_0_0Parser.VariableContext context)
        {
            base.EnterVariable(context);
        }

        public override void ExitAnnotated_formula([NotNull] tptp_v7_0_0_0Parser.Annotated_formulaContext context)
        {
            base.ExitAnnotated_formula(context);
        }

        public override void ExitAnnotations([NotNull] tptp_v7_0_0_0Parser.AnnotationsContext context)
        {
            base.ExitAnnotations(context);
        }

        public override void ExitAssoc_connective([NotNull] tptp_v7_0_0_0Parser.Assoc_connectiveContext context)
        {
            base.ExitAssoc_connective(context);
        }

        public override void ExitAssumptions_record([NotNull] tptp_v7_0_0_0Parser.Assumptions_recordContext context)
        {
            base.ExitAssumptions_record(context);
        }

        public override void ExitAtom([NotNull] tptp_v7_0_0_0Parser.AtomContext context)
        {
            base.ExitAtom(context);
        }

        public override void ExitAtomic_defined_word([NotNull] tptp_v7_0_0_0Parser.Atomic_defined_wordContext context)
        {
            base.ExitAtomic_defined_word(context);
        }

        public override void ExitAtomic_system_word([NotNull] tptp_v7_0_0_0Parser.Atomic_system_wordContext context)
        {
            base.ExitAtomic_system_word(context);
        }

        public override void ExitAtomic_word([NotNull] tptp_v7_0_0_0Parser.Atomic_wordContext context)
        {
            base.ExitAtomic_word(context);
        }

        public override void ExitBinary_connective([NotNull] tptp_v7_0_0_0Parser.Binary_connectiveContext context)
        {
            base.ExitBinary_connective(context);
        }

        public override void ExitCnf_annotated([NotNull] tptp_v7_0_0_0Parser.Cnf_annotatedContext context)
        {
            base.ExitCnf_annotated(context);
        }

        public override void ExitCnf_disjunction([NotNull] tptp_v7_0_0_0Parser.Cnf_disjunctionContext context)
        {
            base.ExitCnf_disjunction(context);
        }

        public override void ExitCnf_formula([NotNull] tptp_v7_0_0_0Parser.Cnf_formulaContext context)
        {
            base.ExitCnf_formula(context);
        }

        public override void ExitCnf_literal([NotNull] tptp_v7_0_0_0Parser.Cnf_literalContext context)
        {
            base.ExitCnf_literal(context);
        }

        public override void ExitConstant([NotNull] tptp_v7_0_0_0Parser.ConstantContext context)
        {
            base.ExitConstant(context);
        }

        public override void ExitCreator_name([NotNull] tptp_v7_0_0_0Parser.Creator_nameContext context)
        {
            base.ExitCreator_name(context);
        }

        public override void ExitCreator_source([NotNull] tptp_v7_0_0_0Parser.Creator_sourceContext context)
        {
            base.ExitCreator_source(context);
        }

        public override void ExitDag_source([NotNull] tptp_v7_0_0_0Parser.Dag_sourceContext context)
        {
            base.ExitDag_source(context);
        }

        public override void ExitDefined_constant([NotNull] tptp_v7_0_0_0Parser.Defined_constantContext context)
        {
            base.ExitDefined_constant(context);
        }

        public override void ExitDefined_functor([NotNull] tptp_v7_0_0_0Parser.Defined_functorContext context)
        {
            base.ExitDefined_functor(context);
        }

        public override void ExitDefined_infix_pred([NotNull] tptp_v7_0_0_0Parser.Defined_infix_predContext context)
        {
            base.ExitDefined_infix_pred(context);
        }

        public override void ExitDefined_predicate([NotNull] tptp_v7_0_0_0Parser.Defined_predicateContext context)
        {
            base.ExitDefined_predicate(context);
        }

        public override void ExitDefined_proposition([NotNull] tptp_v7_0_0_0Parser.Defined_propositionContext context)
        {
            base.ExitDefined_proposition(context);
        }

        public override void ExitDefined_term([NotNull] tptp_v7_0_0_0Parser.Defined_termContext context)
        {
            base.ExitDefined_term(context);
        }

        public override void ExitDefined_type([NotNull] tptp_v7_0_0_0Parser.Defined_typeContext context)
        {
            base.ExitDefined_type(context);
        }

        public override void ExitDescription_item([NotNull] tptp_v7_0_0_0Parser.Description_itemContext context)
        {
            base.ExitDescription_item(context);
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            base.ExitEveryRule(context);
        }

        public override void ExitExternal_source([NotNull] tptp_v7_0_0_0Parser.External_sourceContext context)
        {
            base.ExitExternal_source(context);
        }

        public override void ExitFile_info([NotNull] tptp_v7_0_0_0Parser.File_infoContext context)
        {
            base.ExitFile_info(context);
        }

        public override void ExitFile_name([NotNull] tptp_v7_0_0_0Parser.File_nameContext context)
        {
            base.ExitFile_name(context);
        }

        public override void ExitFile_source([NotNull] tptp_v7_0_0_0Parser.File_sourceContext context)
        {
            base.ExitFile_source(context);
        }

        public override void ExitFof_and_formula([NotNull] tptp_v7_0_0_0Parser.Fof_and_formulaContext context)
        {
            base.ExitFof_and_formula(context);
        }

        public override void ExitFof_annotated([NotNull] tptp_v7_0_0_0Parser.Fof_annotatedContext context)
        {
            base.ExitFof_annotated(context);
        }

        public override void ExitFof_arguments([NotNull] tptp_v7_0_0_0Parser.Fof_argumentsContext context)
        {
            base.ExitFof_arguments(context);
        }

        public override void ExitFof_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_atomic_formulaContext context)
        {
            base.ExitFof_atomic_formula(context);
        }

        public override void ExitFof_binary_assoc([NotNull] tptp_v7_0_0_0Parser.Fof_binary_assocContext context)
        {
            base.ExitFof_binary_assoc(context);
        }

        public override void ExitFof_binary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_binary_formulaContext context)
        {
            base.ExitFof_binary_formula(context);
        }

        public override void ExitFof_binary_nonassoc([NotNull] tptp_v7_0_0_0Parser.Fof_binary_nonassocContext context)
        {
            base.ExitFof_binary_nonassoc(context);
        }

        public override void ExitFof_defined_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_atomic_formulaContext context)
        {
            base.ExitFof_defined_atomic_formula(context);
        }

        public override void ExitFof_defined_atomic_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_atomic_termContext context)
        {
            base.ExitFof_defined_atomic_term(context);
        }

        public override void ExitFof_defined_infix_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_infix_formulaContext context)
        {
            base.ExitFof_defined_infix_formula(context);
        }

        public override void ExitFof_defined_plain_formula([NotNull] tptp_v7_0_0_0Parser.Fof_defined_plain_formulaContext context)
        {
            base.ExitFof_defined_plain_formula(context);
        }

        public override void ExitFof_defined_plain_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_plain_termContext context)
        {
            base.ExitFof_defined_plain_term(context);
        }

        public override void ExitFof_defined_term([NotNull] tptp_v7_0_0_0Parser.Fof_defined_termContext context)
        {
            base.ExitFof_defined_term(context);
        }

        public override void ExitFof_formula([NotNull] tptp_v7_0_0_0Parser.Fof_formulaContext context)
        {
            base.ExitFof_formula(context);
        }

        public override void ExitFof_formula_tuple([NotNull] tptp_v7_0_0_0Parser.Fof_formula_tupleContext context)
        {
            base.ExitFof_formula_tuple(context);
        }

        public override void ExitFof_formula_tuple_list([NotNull] tptp_v7_0_0_0Parser.Fof_formula_tuple_listContext context)
        {
            base.ExitFof_formula_tuple_list(context);
        }

        public override void ExitFof_function_term([NotNull] tptp_v7_0_0_0Parser.Fof_function_termContext context)
        {
            base.ExitFof_function_term(context);
        }

        public override void ExitFof_infix_unary([NotNull] tptp_v7_0_0_0Parser.Fof_infix_unaryContext context)
        {
            base.ExitFof_infix_unary(context);
        }

        public override void ExitFof_logic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_logic_formulaContext context)
        {
            base.ExitFof_logic_formula(context);
        }

        public override void ExitFof_or_formula([NotNull] tptp_v7_0_0_0Parser.Fof_or_formulaContext context)
        {
            base.ExitFof_or_formula(context);
        }

        public override void ExitFof_plain_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_plain_atomic_formulaContext context)
        {
            base.ExitFof_plain_atomic_formula(context);
        }

        public override void ExitFof_plain_term([NotNull] tptp_v7_0_0_0Parser.Fof_plain_termContext context)
        {
            base.ExitFof_plain_term(context);
        }

        public override void ExitFof_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Fof_quantified_formulaContext context)
        {
            base.ExitFof_quantified_formula(context);
        }

        public override void ExitFof_quantifier([NotNull] tptp_v7_0_0_0Parser.Fof_quantifierContext context)
        {
            base.ExitFof_quantifier(context);
        }

        public override void ExitFof_sequent([NotNull] tptp_v7_0_0_0Parser.Fof_sequentContext context)
        {
            base.ExitFof_sequent(context);
        }

        public override void ExitFof_system_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Fof_system_atomic_formulaContext context)
        {
            base.ExitFof_system_atomic_formula(context);
        }

        public override void ExitFof_system_term([NotNull] tptp_v7_0_0_0Parser.Fof_system_termContext context)
        {
            base.ExitFof_system_term(context);
        }

        public override void ExitFof_term([NotNull] tptp_v7_0_0_0Parser.Fof_termContext context)
        {
            base.ExitFof_term(context);
        }

        public override void ExitFof_unary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_unary_formulaContext context)
        {
            base.ExitFof_unary_formula(context);
        }

        public override void ExitFof_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Fof_unitary_formulaContext context)
        {
            base.ExitFof_unitary_formula(context);
        }

        public override void ExitFof_variable_list([NotNull] tptp_v7_0_0_0Parser.Fof_variable_listContext context)
        {
            base.ExitFof_variable_list(context);
        }

        public override void ExitFormula_data([NotNull] tptp_v7_0_0_0Parser.Formula_dataContext context)
        {
            base.ExitFormula_data(context);
        }

        public override void ExitFormula_item([NotNull] tptp_v7_0_0_0Parser.Formula_itemContext context)
        {
            base.ExitFormula_item(context);
        }

        public override void ExitFormula_role([NotNull] tptp_v7_0_0_0Parser.Formula_roleContext context)
        {
            base.ExitFormula_role(context);
        }

        public override void ExitFormula_selection([NotNull] tptp_v7_0_0_0Parser.Formula_selectionContext context)
        {
            base.ExitFormula_selection(context);
        }

        public override void ExitFunctor([NotNull] tptp_v7_0_0_0Parser.FunctorContext context)
        {
            base.ExitFunctor(context);
        }

        public override void ExitGeneral_data([NotNull] tptp_v7_0_0_0Parser.General_dataContext context)
        {
            base.ExitGeneral_data(context);
        }

        public override void ExitGeneral_function([NotNull] tptp_v7_0_0_0Parser.General_functionContext context)
        {
            base.ExitGeneral_function(context);
        }

        public override void ExitGeneral_list([NotNull] tptp_v7_0_0_0Parser.General_listContext context)
        {
            base.ExitGeneral_list(context);
        }

        public override void ExitGeneral_term([NotNull] tptp_v7_0_0_0Parser.General_termContext context)
        {
            base.ExitGeneral_term(context);
        }

        public override void ExitGeneral_terms([NotNull] tptp_v7_0_0_0Parser.General_termsContext context)
        {
            base.ExitGeneral_terms(context);
        }

        public override void ExitInclude([NotNull] tptp_v7_0_0_0Parser.IncludeContext context)
        {
            base.ExitInclude(context);
        }

        public override void ExitInference_info([NotNull] tptp_v7_0_0_0Parser.Inference_infoContext context)
        {
            base.ExitInference_info(context);
        }

        public override void ExitInference_item([NotNull] tptp_v7_0_0_0Parser.Inference_itemContext context)
        {
            base.ExitInference_item(context);
        }

        public override void ExitInference_parents([NotNull] tptp_v7_0_0_0Parser.Inference_parentsContext context)
        {
            base.ExitInference_parents(context);
        }

        public override void ExitInference_record([NotNull] tptp_v7_0_0_0Parser.Inference_recordContext context)
        {
            base.ExitInference_record(context);
        }

        public override void ExitInference_rule([NotNull] tptp_v7_0_0_0Parser.Inference_ruleContext context)
        {
            base.ExitInference_rule(context);
        }

        public override void ExitInference_status([NotNull] tptp_v7_0_0_0Parser.Inference_statusContext context)
        {
            base.ExitInference_status(context);
        }

        public override void ExitInfo_item([NotNull] tptp_v7_0_0_0Parser.Info_itemContext context)
        {
            base.ExitInfo_item(context);
        }

        public override void ExitInfo_items([NotNull] tptp_v7_0_0_0Parser.Info_itemsContext context)
        {
            base.ExitInfo_items(context);
        }

        public override void ExitInternal_source([NotNull] tptp_v7_0_0_0Parser.Internal_sourceContext context)
        {
            base.ExitInternal_source(context);
        }

        public override void ExitIntro_type([NotNull] tptp_v7_0_0_0Parser.Intro_typeContext context)
        {
            base.ExitIntro_type(context);
        }

        public override void ExitIquote_item([NotNull] tptp_v7_0_0_0Parser.Iquote_itemContext context)
        {
            base.ExitIquote_item(context);
        }

        public override void ExitName([NotNull] tptp_v7_0_0_0Parser.NameContext context)
        {
            base.ExitName(context);
        }

        public override void ExitName_list([NotNull] tptp_v7_0_0_0Parser.Name_listContext context)
        {
            base.ExitName_list(context);
        }

        public override void ExitNew_symbol_list([NotNull] tptp_v7_0_0_0Parser.New_symbol_listContext context)
        {
            base.ExitNew_symbol_list(context);
        }

        public override void ExitNew_symbol_record([NotNull] tptp_v7_0_0_0Parser.New_symbol_recordContext context)
        {
            base.ExitNew_symbol_record(context);
        }

        public override void ExitNumber([NotNull] tptp_v7_0_0_0Parser.NumberContext context)
        {
            base.ExitNumber(context);
        }

        public override void ExitOptional_info([NotNull] tptp_v7_0_0_0Parser.Optional_infoContext context)
        {
            base.ExitOptional_info(context);
        }

        public override void ExitParent_details([NotNull] tptp_v7_0_0_0Parser.Parent_detailsContext context)
        {
            base.ExitParent_details(context);
        }

        public override void ExitParent_info([NotNull] tptp_v7_0_0_0Parser.Parent_infoContext context)
        {
            base.ExitParent_info(context);
        }

        public override void ExitParent_list([NotNull] tptp_v7_0_0_0Parser.Parent_listContext context)
        {
            base.ExitParent_list(context);
        }

        public override void ExitPrincipal_symbol([NotNull] tptp_v7_0_0_0Parser.Principal_symbolContext context)
        {
            base.ExitPrincipal_symbol(context);
        }

        public override void ExitRefutation([NotNull] tptp_v7_0_0_0Parser.RefutationContext context)
        {
            base.ExitRefutation(context);
        }

        public override void ExitSource([NotNull] tptp_v7_0_0_0Parser.SourceContext context)
        {
            base.ExitSource(context);
        }

        public override void ExitSources([NotNull] tptp_v7_0_0_0Parser.SourcesContext context)
        {
            base.ExitSources(context);
        }

        public override void ExitStatus_value([NotNull] tptp_v7_0_0_0Parser.Status_valueContext context)
        {
            base.ExitStatus_value(context);
        }

        public override void ExitSystem_constant([NotNull] tptp_v7_0_0_0Parser.System_constantContext context)
        {
            base.ExitSystem_constant(context);
        }

        public override void ExitSystem_functor([NotNull] tptp_v7_0_0_0Parser.System_functorContext context)
        {
            base.ExitSystem_functor(context);
        }

        public override void ExitSystem_type([NotNull] tptp_v7_0_0_0Parser.System_typeContext context)
        {
            base.ExitSystem_type(context);
        }

        public override void ExitTcf_annotated([NotNull] tptp_v7_0_0_0Parser.Tcf_annotatedContext context)
        {
            base.ExitTcf_annotated(context);
        }

        public override void ExitTcf_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_formulaContext context)
        {
            base.ExitTcf_formula(context);
        }

        public override void ExitTcf_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_logic_formulaContext context)
        {
            base.ExitTcf_logic_formula(context);
        }

        public override void ExitTcf_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Tcf_quantified_formulaContext context)
        {
            base.ExitTcf_quantified_formula(context);
        }

        public override void ExitTf1_quantified_type([NotNull] tptp_v7_0_0_0Parser.Tf1_quantified_typeContext context)
        {
            base.ExitTf1_quantified_type(context);
        }

        public override void ExitTff_and_formula([NotNull] tptp_v7_0_0_0Parser.Tff_and_formulaContext context)
        {
            base.ExitTff_and_formula(context);
        }

        public override void ExitTff_annotated([NotNull] tptp_v7_0_0_0Parser.Tff_annotatedContext context)
        {
            base.ExitTff_annotated(context);
        }

        public override void ExitTff_atomic_formula([NotNull] tptp_v7_0_0_0Parser.Tff_atomic_formulaContext context)
        {
            base.ExitTff_atomic_formula(context);
        }

        public override void ExitTff_atomic_type([NotNull] tptp_v7_0_0_0Parser.Tff_atomic_typeContext context)
        {
            base.ExitTff_atomic_type(context);
        }

        public override void ExitTff_binary_assoc([NotNull] tptp_v7_0_0_0Parser.Tff_binary_assocContext context)
        {
            base.ExitTff_binary_assoc(context);
        }

        public override void ExitTff_binary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_binary_formulaContext context)
        {
            base.ExitTff_binary_formula(context);
        }

        public override void ExitTff_binary_nonassoc([NotNull] tptp_v7_0_0_0Parser.Tff_binary_nonassocContext context)
        {
            base.ExitTff_binary_nonassoc(context);
        }

        public override void ExitTff_conditional([NotNull] tptp_v7_0_0_0Parser.Tff_conditionalContext context)
        {
            base.ExitTff_conditional(context);
        }

        public override void ExitTff_conditional_term([NotNull] tptp_v7_0_0_0Parser.Tff_conditional_termContext context)
        {
            base.ExitTff_conditional_term(context);
        }

        public override void ExitTff_formula([NotNull] tptp_v7_0_0_0Parser.Tff_formulaContext context)
        {
            base.ExitTff_formula(context);
        }

        public override void ExitTff_formula_tuple([NotNull] tptp_v7_0_0_0Parser.Tff_formula_tupleContext context)
        {
            base.ExitTff_formula_tuple(context);
        }

        public override void ExitTff_formula_tuple_list([NotNull] tptp_v7_0_0_0Parser.Tff_formula_tuple_listContext context)
        {
            base.ExitTff_formula_tuple_list(context);
        }

        public override void ExitTff_let([NotNull] tptp_v7_0_0_0Parser.Tff_letContext context)
        {
            base.ExitTff_let(context);
        }

        public override void ExitTff_let_formula_binding([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_bindingContext context)
        {
            base.ExitTff_let_formula_binding(context);
        }

        public override void ExitTff_let_formula_defn([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_defnContext context)
        {
            base.ExitTff_let_formula_defn(context);
        }

        public override void ExitTff_let_formula_defns([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_defnsContext context)
        {
            base.ExitTff_let_formula_defns(context);
        }

        public override void ExitTff_let_formula_list([NotNull] tptp_v7_0_0_0Parser.Tff_let_formula_listContext context)
        {
            base.ExitTff_let_formula_list(context);
        }

        public override void ExitTff_let_term([NotNull] tptp_v7_0_0_0Parser.Tff_let_termContext context)
        {
            base.ExitTff_let_term(context);
        }

        public override void ExitTff_let_term_binding([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_bindingContext context)
        {
            base.ExitTff_let_term_binding(context);
        }

        public override void ExitTff_let_term_defn([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_defnContext context)
        {
            base.ExitTff_let_term_defn(context);
        }

        public override void ExitTff_let_term_defns([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_defnsContext context)
        {
            base.ExitTff_let_term_defns(context);
        }

        public override void ExitTff_let_term_list([NotNull] tptp_v7_0_0_0Parser.Tff_let_term_listContext context)
        {
            base.ExitTff_let_term_list(context);
        }

        public override void ExitTff_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tff_logic_formulaContext context)
        {
            base.ExitTff_logic_formula(context);
        }

        public override void ExitTff_mapping_type([NotNull] tptp_v7_0_0_0Parser.Tff_mapping_typeContext context)
        {
            base.ExitTff_mapping_type(context);
        }

        public override void ExitTff_monotype([NotNull] tptp_v7_0_0_0Parser.Tff_monotypeContext context)
        {
            base.ExitTff_monotype(context);
        }

        public override void ExitTff_or_formula([NotNull] tptp_v7_0_0_0Parser.Tff_or_formulaContext context)
        {
            base.ExitTff_or_formula(context);
        }

        public override void ExitTff_pair_connective([NotNull] tptp_v7_0_0_0Parser.Tff_pair_connectiveContext context)
        {
            base.ExitTff_pair_connective(context);
        }

        public override void ExitTff_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Tff_quantified_formulaContext context)
        {
            base.ExitTff_quantified_formula(context);
        }

        public override void ExitTff_sequent([NotNull] tptp_v7_0_0_0Parser.Tff_sequentContext context)
        {
            base.ExitTff_sequent(context);
        }

        public override void ExitTff_subtype([NotNull] tptp_v7_0_0_0Parser.Tff_subtypeContext context)
        {
            base.ExitTff_subtype(context);
        }

        public override void ExitTff_top_level_type([NotNull] tptp_v7_0_0_0Parser.Tff_top_level_typeContext context)
        {
            base.ExitTff_top_level_type(context);
        }

        public override void ExitTff_tuple_term([NotNull] tptp_v7_0_0_0Parser.Tff_tuple_termContext context)
        {
            base.ExitTff_tuple_term(context);
        }

        public override void ExitTff_typed_atom([NotNull] tptp_v7_0_0_0Parser.Tff_typed_atomContext context)
        {
            base.ExitTff_typed_atom(context);
        }

        public override void ExitTff_typed_variable([NotNull] tptp_v7_0_0_0Parser.Tff_typed_variableContext context)
        {
            base.ExitTff_typed_variable(context);
        }

        public override void ExitTff_type_arguments([NotNull] tptp_v7_0_0_0Parser.Tff_type_argumentsContext context)
        {
            base.ExitTff_type_arguments(context);
        }

        public override void ExitTff_unary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_unary_formulaContext context)
        {
            base.ExitTff_unary_formula(context);
        }

        public override void ExitTff_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Tff_unitary_formulaContext context)
        {
            base.ExitTff_unitary_formula(context);
        }

        public override void ExitTff_unitary_type([NotNull] tptp_v7_0_0_0Parser.Tff_unitary_typeContext context)
        {
            base.ExitTff_unitary_type(context);
        }

        public override void ExitTff_variable([NotNull] tptp_v7_0_0_0Parser.Tff_variableContext context)
        {
            base.ExitTff_variable(context);
        }

        public override void ExitTff_variable_list([NotNull] tptp_v7_0_0_0Parser.Tff_variable_listContext context)
        {
            base.ExitTff_variable_list(context);
        }

        public override void ExitTff_xprod_type([NotNull] tptp_v7_0_0_0Parser.Tff_xprod_typeContext context)
        {
            base.ExitTff_xprod_type(context);
        }

        public override void ExitTfx_annotated([NotNull] tptp_v7_0_0_0Parser.Tfx_annotatedContext context)
        {
            base.ExitTfx_annotated(context);
        }

        public override void ExitTfx_formula([NotNull] tptp_v7_0_0_0Parser.Tfx_formulaContext context)
        {
            base.ExitTfx_formula(context);
        }

        public override void ExitTfx_logic_formula([NotNull] tptp_v7_0_0_0Parser.Tfx_logic_formulaContext context)
        {
            base.ExitTfx_logic_formula(context);
        }

        public override void ExitTh0_quantifier([NotNull] tptp_v7_0_0_0Parser.Th0_quantifierContext context)
        {
            base.ExitTh0_quantifier(context);
        }

        public override void ExitTh1_quantifier([NotNull] tptp_v7_0_0_0Parser.Th1_quantifierContext context)
        {
            base.ExitTh1_quantifier(context);
        }

        public override void ExitTh1_unary_connective([NotNull] tptp_v7_0_0_0Parser.Th1_unary_connectiveContext context)
        {
            base.ExitTh1_unary_connective(context);
        }

        public override void ExitTheory([NotNull] tptp_v7_0_0_0Parser.TheoryContext context)
        {
            base.ExitTheory(context);
        }

        public override void ExitTheory_name([NotNull] tptp_v7_0_0_0Parser.Theory_nameContext context)
        {
            base.ExitTheory_name(context);
        }

        public override void ExitThf_and_formula([NotNull] tptp_v7_0_0_0Parser.Thf_and_formulaContext context)
        {
            base.ExitThf_and_formula(context);
        }

        public override void ExitThf_annotated([NotNull] tptp_v7_0_0_0Parser.Thf_annotatedContext context)
        {
            base.ExitThf_annotated(context);
        }

        public override void ExitThf_apply_formula([NotNull] tptp_v7_0_0_0Parser.Thf_apply_formulaContext context)
        {
            base.ExitThf_apply_formula(context);
        }

        public override void ExitThf_apply_type([NotNull] tptp_v7_0_0_0Parser.Thf_apply_typeContext context)
        {
            base.ExitThf_apply_type(context);
        }

        public override void ExitThf_arguments([NotNull] tptp_v7_0_0_0Parser.Thf_argumentsContext context)
        {
            base.ExitThf_arguments(context);
        }

        public override void ExitThf_atom([NotNull] tptp_v7_0_0_0Parser.Thf_atomContext context)
        {
            base.ExitThf_atom(context);
        }

        public override void ExitThf_binary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_binary_formulaContext context)
        {
            base.ExitThf_binary_formula(context);
        }

        public override void ExitThf_binary_pair([NotNull] tptp_v7_0_0_0Parser.Thf_binary_pairContext context)
        {
            base.ExitThf_binary_pair(context);
        }

        public override void ExitThf_binary_tuple([NotNull] tptp_v7_0_0_0Parser.Thf_binary_tupleContext context)
        {
            base.ExitThf_binary_tuple(context);
        }

        public override void ExitThf_binary_type([NotNull] tptp_v7_0_0_0Parser.Thf_binary_typeContext context)
        {
            base.ExitThf_binary_type(context);
        }

        public override void ExitThf_conditional([NotNull] tptp_v7_0_0_0Parser.Thf_conditionalContext context)
        {
            base.ExitThf_conditional(context);
        }

        public override void ExitThf_conn_term([NotNull] tptp_v7_0_0_0Parser.Thf_conn_termContext context)
        {
            base.ExitThf_conn_term(context);
        }

        public override void ExitThf_formula([NotNull] tptp_v7_0_0_0Parser.Thf_formulaContext context)
        {
            base.ExitThf_formula(context);
        }

        public override void ExitThf_formula_list([NotNull] tptp_v7_0_0_0Parser.Thf_formula_listContext context)
        {
            base.ExitThf_formula_list(context);
        }

        public override void ExitThf_function([NotNull] tptp_v7_0_0_0Parser.Thf_functionContext context)
        {
            base.ExitThf_function(context);
        }

        public override void ExitThf_let([NotNull] tptp_v7_0_0_0Parser.Thf_letContext context)
        {
            base.ExitThf_let(context);
        }

        public override void ExitThf_logic_formula([NotNull] tptp_v7_0_0_0Parser.Thf_logic_formulaContext context)
        {
            base.ExitThf_logic_formula(context);
        }

        public override void ExitThf_mapping_type([NotNull] tptp_v7_0_0_0Parser.Thf_mapping_typeContext context)
        {
            base.ExitThf_mapping_type(context);
        }

        public override void ExitThf_or_formula([NotNull] tptp_v7_0_0_0Parser.Thf_or_formulaContext context)
        {
            base.ExitThf_or_formula(context);
        }

        public override void ExitThf_pair_connective([NotNull] tptp_v7_0_0_0Parser.Thf_pair_connectiveContext context)
        {
            base.ExitThf_pair_connective(context);
        }

        public override void ExitThf_quantification([NotNull] tptp_v7_0_0_0Parser.Thf_quantificationContext context)
        {
            base.ExitThf_quantification(context);
        }

        public override void ExitThf_quantified_formula([NotNull] tptp_v7_0_0_0Parser.Thf_quantified_formulaContext context)
        {
            base.ExitThf_quantified_formula(context);
        }

        public override void ExitThf_quantifier([NotNull] tptp_v7_0_0_0Parser.Thf_quantifierContext context)
        {
            base.ExitThf_quantifier(context);
        }

        public override void ExitThf_sequent([NotNull] tptp_v7_0_0_0Parser.Thf_sequentContext context)
        {
            base.ExitThf_sequent(context);
        }

        public override void ExitThf_subtype([NotNull] tptp_v7_0_0_0Parser.Thf_subtypeContext context)
        {
            base.ExitThf_subtype(context);
        }

        public override void ExitThf_top_level_type([NotNull] tptp_v7_0_0_0Parser.Thf_top_level_typeContext context)
        {
            base.ExitThf_top_level_type(context);
        }

        public override void ExitThf_tuple([NotNull] tptp_v7_0_0_0Parser.Thf_tupleContext context)
        {
            base.ExitThf_tuple(context);
        }

        public override void ExitThf_typeable_formula([NotNull] tptp_v7_0_0_0Parser.Thf_typeable_formulaContext context)
        {
            base.ExitThf_typeable_formula(context);
        }

        public override void ExitThf_typed_variable([NotNull] tptp_v7_0_0_0Parser.Thf_typed_variableContext context)
        {
            base.ExitThf_typed_variable(context);
        }

        public override void ExitThf_type_formula([NotNull] tptp_v7_0_0_0Parser.Thf_type_formulaContext context)
        {
            base.ExitThf_type_formula(context);
        }

        public override void ExitThf_unary_connective([NotNull] tptp_v7_0_0_0Parser.Thf_unary_connectiveContext context)
        {
            base.ExitThf_unary_connective(context);
        }

        public override void ExitThf_unary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_unary_formulaContext context)
        {
            base.ExitThf_unary_formula(context);
        }

        public override void ExitThf_union_type([NotNull] tptp_v7_0_0_0Parser.Thf_union_typeContext context)
        {
            base.ExitThf_union_type(context);
        }

        public override void ExitThf_unitary_formula([NotNull] tptp_v7_0_0_0Parser.Thf_unitary_formulaContext context)
        {
            base.ExitThf_unitary_formula(context);
        }

        public override void ExitThf_unitary_type([NotNull] tptp_v7_0_0_0Parser.Thf_unitary_typeContext context)
        {
            base.ExitThf_unitary_type(context);
        }

        public override void ExitThf_variable([NotNull] tptp_v7_0_0_0Parser.Thf_variableContext context)
        {
            base.ExitThf_variable(context);
        }

        public override void ExitThf_xprod_type([NotNull] tptp_v7_0_0_0Parser.Thf_xprod_typeContext context)
        {
            base.ExitThf_xprod_type(context);
        }

        public override void ExitTpi_annotated([NotNull] tptp_v7_0_0_0Parser.Tpi_annotatedContext context)
        {
            base.ExitTpi_annotated(context);
        }

        public override void ExitTpi_formula([NotNull] tptp_v7_0_0_0Parser.Tpi_formulaContext context)
        {
            base.ExitTpi_formula(context);
        }

        public override void ExitTptp_file([NotNull] tptp_v7_0_0_0Parser.Tptp_fileContext context)
        {
            base.ExitTptp_file(context);
        }

        public override void ExitTptp_input([NotNull] tptp_v7_0_0_0Parser.Tptp_inputContext context)
        {
            base.ExitTptp_input(context);
        }

        public override void ExitType_constant([NotNull] tptp_v7_0_0_0Parser.Type_constantContext context)
        {
            base.ExitType_constant(context);
        }

        public override void ExitType_functor([NotNull] tptp_v7_0_0_0Parser.Type_functorContext context)
        {
            base.ExitType_functor(context);
        }

        public override void ExitUnary_connective([NotNull] tptp_v7_0_0_0Parser.Unary_connectiveContext context)
        {
            base.ExitUnary_connective(context);
        }

        public override void ExitUntyped_atom([NotNull] tptp_v7_0_0_0Parser.Untyped_atomContext context)
        {
            base.ExitUntyped_atom(context);
        }

        public override void ExitUseful_info([NotNull] tptp_v7_0_0_0Parser.Useful_infoContext context)
        {
            base.ExitUseful_info(context);
        }

        public override void ExitVariable([NotNull] tptp_v7_0_0_0Parser.VariableContext context)
        {
            base.ExitVariable(context);
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
            base.VisitErrorNode(node);
        }

        public override void VisitTerminal([NotNull] ITerminalNode node)
        {
            base.VisitTerminal(node);
        }
    }
}
