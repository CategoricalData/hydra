module Hydra.Ext.Demos.GenPG.ExampleMapping where

import Hydra.Core (Term)
import Hydra.Pg.Model (Edge, Vertex)
import Hydra.Formatting (decapitalize)
import Hydra.Phantoms (TTerm)
import Hydra.Dsl.Meta.Phantoms ((@@), constant, just, lambda, nothing, string, var)
import Hydra.Ext.Dsl.Pg.Mappings (LazyGraph, column, edge, edgeNoId, graph, property, vertex)
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Lib.Strings as Strings
import Hydra.Ext.Demos.GenPG.ExampleGraphSchema

-- Helpers -----------------------

labeledIntId :: String -> TTerm (r -> Maybe Int) -> TTerm (r -> String)
labeledIntId itype iid = lambda "r" $ Maybes.map
  (lambda "i" $ Strings.concat [
    string $ decapitalize itype,
    string "_",
    Literals.showInt32 $ var "i"])
  (iid @@ var "r")

interactionVertex :: String -> String -> Vertex Term
interactionVertex tableName itype = vertex tableName customerInteractionVertexLabel
  (labeledIntId itype (column "id"))
  [property "interactionType" $ constant $ just $ string itype,
   property "interactionDate" $ column "interaction_date",
   property "notes" $ column "notes",
   property "durationMinutes" $ column "duration_minutes",
   property "followUpRequired" $ column "follow_up_required",
   property "customerInitiated" $ column "customer_initiated"]

employeeEdge :: String -> String -> Edge Term
employeeEdge tableName itype = edgeNoId tableName employeeEdgeLabel
  (labeledIntId itype (column "id"))
  (labeledIntId employeeVertexLabel $ column "employee_id")
  []

customerEdge :: String -> String -> Edge Term
customerEdge tableName itype = edgeNoId tableName customerEdgeLabel
  (labeledIntId itype (column "id"))
  (labeledIntId customerVertexLabel $ column "customer_id")
  []

-- Mapping -----------------------------

salesGraph :: LazyGraph Term
salesGraph = graph
  -- Vertices
  [vertex "employees.csv" employeeVertexLabel (labeledIntId employeeVertexLabel $ column "employee_id") [
     property "firstName" $ column "first_name",
     property "lastName" $ column "last_name",
     property "email" $ column "email",
     property "hireDate" $ column "hire_date"],

   vertex "departments.csv" departmentVertexLabel (labeledIntId departmentVertexLabel $ column "department_id") [
     property "name" $ column "department_name"],

   vertex "customers.csv" customerVertexLabel (labeledIntId customerVertexLabel $ column "customer_id") [
     property "companyName" $ column "company_name",
     property "contactName" $ column "contact_name",
     property "email" $ column "email",
     property "phone" $ column "phone"],

   vertex "products.csv" productVertexLabel (labeledIntId productVertexLabel $ column "product_id") [
     property "name" $ column "product_name",
     property "price" $ column "price"],

   vertex "sales.csv" saleVertexLabel (labeledIntId saleVertexLabel $ column "sale_id") [
     property "saleDate" $ column "sale_date",
     property "totalAmount" $ column "total_amount"],

   vertex "sale_items.csv" saleItemVertexLabel (labeledIntId saleItemVertexLabel $ column "sale_item_id") [
     property "quantity" $ column "quantity",
     property "itemPrice" $ column "item_price"],

   interactionVertex "calls.csv" callInteractionType,
   interactionVertex "emails.csv" emailInteractionType,
   interactionVertex "meetings.csv" meetingInteractionType]

  -- Edges
  [edgeNoId "employees.csv" managesEdgeLabel
     (labeledIntId employeeVertexLabel $ column "manager_id")
     (labeledIntId employeeVertexLabel $ column "employee_id")
     [property "sinceDate" $ column "management_since_date"],

   edgeNoId "employees.csv" belongsToEdgeLabel
     (labeledIntId employeeVertexLabel $ column "employee_id")
     (labeledIntId departmentVertexLabel $ column "department_id")
     [property "joinDate" $ column "department_join_date",
      property "role" $ column "department_role"],

   edgeNoId "departments.csv" parentDepartmentEdgeLabel
     (labeledIntId departmentVertexLabel $ column "department_id")
     (labeledIntId departmentVertexLabel $ column "parent_department_id")
     [property "since" $ column "parent_department_since"],

   edgeNoId "sales.csv" soldEdgeLabel
     (labeledIntId employeeVertexLabel $ column "employee_id")
     (labeledIntId saleVertexLabel $ column "sale_id")
     [property "commissionRate" $ column "commission_rate",
      property "salesChannel" $ column "sales_channel"],

   edgeNoId "sales.csv" purchasedEdgeLabel
     (labeledIntId customerVertexLabel $ column "customer_id")
     (labeledIntId saleVertexLabel $ column "sale_id")
     [property "paymentMethod" $ column "payment_method",
      property "satisfactionRating" $ column "satisfaction_rating",
      property "isRepeatCustomer" $ column "is_repeat_customer"],

   edgeNoId "sale_items.csv" includesEdgeLabel
     (labeledIntId saleVertexLabel $ column "sale_id")
     (labeledIntId saleItemVertexLabel $ column "sale_item_id")
     [property "itemOrder" $ column "item_order",
      property "discountApplied" $ column "discount_applied"],

   edgeNoId "sale_items.csv" containsProductEdgeLabel
     (labeledIntId saleItemVertexLabel $ column "sale_item_id")
     (labeledIntId productVertexLabel $ column "product_id")
     [property "warrantyPeriodYears" $ column "warranty_period"],

   employeeEdge "calls.csv" callInteractionType,
   customerEdge "calls.csv" callInteractionType,
   employeeEdge "emails.csv" emailInteractionType,
   customerEdge "emails.csv" emailInteractionType,
   employeeEdge "meetings.csv" meetingInteractionType,
   customerEdge "meetings.csv" meetingInteractionType]
