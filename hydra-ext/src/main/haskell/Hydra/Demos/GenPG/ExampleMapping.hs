module Hydra.Demos.GenPG.ExampleMapping where

import Hydra.Core (Term)
import Hydra.Pg.Model (Edge, Vertex)
import Hydra.Formatting (decapitalize)
import Hydra.Phantoms (TTerm)
import Hydra.Dsl.Phantoms ((@@), constant, just, lambda, nothing, string, var)
import Hydra.Dsl.Pg.Mappings (LazyGraph, columnValue, graph, property, simpleEdge, vertex)
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Strings as Strings
import Hydra.Demos.GenPG.ExampleGraphSchema

-- Columns -----------------------------

callsColumn = columnValue "calls.csv"
customersColumn = columnValue "customers.csv"
departmentsColumn = columnValue "departments.csv"
emailsColumn = columnValue "emails.csv"
employeesColumn = columnValue "employees.csv"
meetingsColumn = columnValue "meetings.csv"
productsColumn = columnValue "products.csv"
saleItemsColumn = columnValue "sale_items.csv"
salesColumn = columnValue "sales.csv"

-- Other helpers -----------------------

intColumnToId :: String -> TTerm (r -> Maybe Int) -> TTerm (r -> String)
intColumnToId itype iid = lambda "r" $ Optionals.map
  (lambda "i" $ Strings.concat [
    string $ decapitalize itype,
    string "_",
    Literals.showInt32 @@ (var "i")])
  (iid @@ var "r")

interactionVertices :: String -> (String -> TTerm (r -> Maybe Int)) -> Vertex Term
interactionVertices itype column = vertex customerInteractionVertexLabel
  (intColumnToId itype (column "id"))
  [property "interactionType" $ constant $ just $ string itype,
   property "interactionDate" $ column "interaction_date",
   property "notes" $ column "notes",
   property "durationMinutes" $ column "duration_minutes",
   property "followUpRequired" $ column "follow_up_required",
   property "customerInitiated" $ column "customer_initiated"]

employeeEdges :: String -> (String -> TTerm (r -> Maybe Int)) -> Edge Term
employeeEdges itype column = simpleEdge employeeEdgeLabel
  (intColumnToId itype (column "id"))
  (intColumnToId employeeVertexLabel $ column "employee_id")
  []

customerEdges :: String -> (String -> TTerm (r -> Maybe Int)) -> Edge Term
customerEdges itype column = simpleEdge customerEdgeLabel
  (intColumnToId itype (column "id"))
  (intColumnToId customerVertexLabel $ column "customer_id")
  []

-- Mapping -----------------------------

employeeVertices :: Vertex Term
employeeVertices = vertex employeeVertexLabel (intColumnToId employeeVertexLabel $ employeesColumn "employee_id") [
  property "firstName" $ employeesColumn "first_name",
  property "lastName" $ employeesColumn "last_name",
  property "email" $ employeesColumn "email",
  property "hireDate" $ employeesColumn "hire_date"]

departmentVertices :: Vertex Term
departmentVertices = vertex departmentVertexLabel (intColumnToId departmentVertexLabel $ departmentsColumn "department_id") [
  property "name" $ departmentsColumn "department_name"]

customerVertices :: Vertex Term
customerVertices = vertex customerVertexLabel (intColumnToId customerVertexLabel $ customersColumn "customer_id") [
  property "companyName" $ customersColumn "company_name",
  property "contactName" $ customersColumn "contact_name",
  property "email" $ customersColumn "email",
  property "phone" $ customersColumn "phone"]

productVertices :: Vertex Term
productVertices = vertex productVertexLabel (intColumnToId productVertexLabel $ productsColumn "product_id") [
  property "name" $ productsColumn "product_name",
  property "price" $ productsColumn "price"]

saleVertices :: Vertex Term
saleVertices = vertex saleVertexLabel (intColumnToId saleVertexLabel $ salesColumn "sale_id") [
  property "saleDate" $ salesColumn "sale_date",
  property "totalAmount" $ salesColumn "total_amount"]

saleItemVertices :: Vertex Term
saleItemVertices = vertex saleItemVertexLabel (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id") [
  property "quantity" $ saleItemsColumn "quantity",
  property "itemPrice" $ saleItemsColumn "item_price"]

callInteractionVertices :: Vertex Term
callInteractionVertices = interactionVertices callInteractionType callsColumn

emailInteractionVertices :: Vertex Term
emailInteractionVertices = interactionVertices emailInteractionType emailsColumn

meetingInteractionVertices :: Vertex Term
meetingInteractionVertices = interactionVertices meetingInteractionType meetingsColumn

managesEdges :: Edge Term
managesEdges = simpleEdge managesEdgeLabel
  (intColumnToId employeeVertexLabel $ employeesColumn "manager_id")
  (intColumnToId employeeVertexLabel $ employeesColumn "employee_id")
  [property "sinceDate" $ employeesColumn "management_since_date"]

belongsToEdges :: Edge Term
belongsToEdges = simpleEdge belongsToEdgeLabel
  (intColumnToId employeeVertexLabel $ employeesColumn "employee_id")
  (intColumnToId departmentVertexLabel $ employeesColumn "department_id")
  [property "joinDate" $ employeesColumn "department_join_date",
   property "role" $ employeesColumn "department_role"]

parentDepartmentEdges :: Edge Term
parentDepartmentEdges = simpleEdge parentDepartmentEdgeLabel
  (intColumnToId departmentVertexLabel $ departmentsColumn "department_id")
  (intColumnToId departmentVertexLabel $ departmentsColumn "parent_department_id")
  [property "since" $ departmentsColumn "parent_department_since"]

soldEdges :: Edge Term
soldEdges = simpleEdge soldEdgeLabel
  (intColumnToId employeeVertexLabel $ salesColumn "employee_id")
  (intColumnToId saleVertexLabel $ salesColumn "sale_id")
  [property "commissionRate" $ salesColumn "commission_rate",
   property "salesChannel" $ salesColumn "sales_channel"]

purchasedEdges :: Edge Term
purchasedEdges = simpleEdge purchasedEdgeLabel
  (intColumnToId customerVertexLabel $ salesColumn "customer_id")
  (intColumnToId saleVertexLabel $ salesColumn "sale_id")
  [property "paymentMethod" $ salesColumn "payment_method",
   property "satisfactionRating" $ salesColumn "satisfaction_rating",
   property "isRepeatCustomer" $ salesColumn "is_repeat_customer"]

includesEdges :: Edge Term
includesEdges = simpleEdge includesEdgeLabel
  (intColumnToId saleVertexLabel $ saleItemsColumn "sale_id")
  (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id")
  [property "itemOrder" $ saleItemsColumn "item_order",
   property "discountApplied" $ saleItemsColumn "discount_applied"]

containsProductEdges :: Edge Term
containsProductEdges = simpleEdge containsProductEdgeLabel
  (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id")
  (intColumnToId productVertexLabel $ saleItemsColumn "product_id")
  [property "warrantyPeriodYears" $ saleItemsColumn "warranty_period"]

callEmployeeEdges :: Edge Term
callEmployeeEdges = employeeEdges callInteractionType callsColumn

callCustomerEdges :: Edge Term
callCustomerEdges = customerEdges callInteractionType callsColumn

emailEmployeeEdges :: Edge Term
emailEmployeeEdges = employeeEdges emailInteractionType emailsColumn

emailCustomerEdges :: Edge Term
emailCustomerEdges = customerEdges emailInteractionType emailsColumn

meetingEmployeeEdges :: Edge Term
meetingEmployeeEdges = employeeEdges meetingInteractionType meetingsColumn

meetingCustomerEdges :: Edge Term
meetingCustomerEdges = customerEdges meetingInteractionType meetingsColumn

salesGraphMapping :: LazyGraph Term
salesGraphMapping = graph
  [employeeVertices, departmentVertices, customerVertices, productVertices,
   saleVertices, saleItemVertices,
   callInteractionVertices, emailInteractionVertices, meetingInteractionVertices]
  [managesEdges, belongsToEdges, parentDepartmentEdges,
   soldEdges, purchasedEdges, includesEdges, containsProductEdges,
   callEmployeeEdges, callCustomerEdges,
   emailEmployeeEdges, emailCustomerEdges,
   meetingEmployeeEdges, meetingCustomerEdges]
