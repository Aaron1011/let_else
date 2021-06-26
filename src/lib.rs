use syn::{ExprLet, Expr, Token, parse_macro_input, Pat, Ident};
use syn::parse::{Parse, ParseStream, Result};
use proc_macro::TokenStream;
use quote::quote;

fn collect_bindings(pat: Pat, bindings: &mut Vec<Ident>) {
    match pat {
        Pat::Box(pat_box) => collect_bindings(*pat_box.pat, bindings),
        Pat::Ident(pat_ident) => bindings.push(pat_ident.ident),
        Pat::Lit(_) | Pat::Path(_) | Pat::Range(_) | Pat::Rest(_) | Pat::Wild(_) => {}
        Pat::Reference(pat_ref) => collect_bindings(*pat_ref.pat, bindings),
        Pat::Slice(pat_slice) => {
            for elem in pat_slice.elems {
                collect_bindings(elem, bindings);
            }
        }
        Pat::Struct(pat_struct) => {
            for field in pat_struct.fields {
                collect_bindings(*field.pat, bindings);
            }
        }
        Pat::Tuple(pat_tuple) => {
             for elem in pat_tuple.elems {
                 collect_bindings(elem, bindings);
             }
        }
        Pat::TupleStruct(pat_struct) => {
            for elem in pat_struct.pat.elems {
                collect_bindings(elem, bindings);
            }
        }
        Pat::Verbatim(_) | Pat::Type(_) | Pat::Macro(_) | Pat::Or(_) | _ => panic!("Unsupported pat {:?}", pat)
    }
}

struct LetElse {
    expr_let: ExprLet,
    else_expr: Expr
}

impl Parse for LetElse {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr_let: ExprLet = input.parse()?;
        input.parse::<Token![else]>()?;
        let else_expr: Expr = input.parse()?;
        Ok(LetElse { expr_let, else_expr })
    }
}

#[proc_macro]
pub fn let_else(input: TokenStream) -> TokenStream {
    let LetElse { expr_let, else_expr } = parse_macro_input!(input as LetElse);
    let pat = expr_let.pat;
    let match_expr = expr_let.expr;
    let mut bindings = Vec::new();
    collect_bindings(pat.clone(), &mut bindings);
    let out = quote! {
        let (#(#bindings),*) = match #match_expr {
            #pat => (#(#bindings),*),
            _ => #else_expr
        };
    };
    out.into()
}
