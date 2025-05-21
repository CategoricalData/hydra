{-# LANGUAGE OverloadedStrings #-}

module Hydra.Demos.PropertyGraphGeneration.ExampleMapping where

import Hydra.Core
import qualified Hydra.Pg.Model as Pg
import Hydra.Formatting
import Hydra.Phantoms
import Hydra.Dsl.Phantoms
import Hydra.Dsl.Pg.Mappings
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Strings as Strings


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

-- Vertex Labels -----------------------

employeeVertexLabel = "Employee"
departmentVertexLabel = "Department"
customerVertexLabel = "Customer"
productVertexLabel = "Product"
saleVertexLabel = "Sale"
saleItemVertexLabel = "SaleItem"
customerInteractionVertexLabel = "CustomerInteraction"

-- Edge Labels -------------------------

managesEdgeLabel = "manages"
belongsToEdgeLabel = "belongsTo"
parentDepartmentEdgeLabel = "parentDepartment"
soldEdgeLabel = "sold"
purchasedEdgeLabel = "purchased"
includesEdgeLabel = "includes"
containsProductEdgeLabel = "containsProduct"
employeeEdgeLabel = "employee"
customerEdgeLabel = "customer"

-- Interaction Types -------------------

callInteractionType = "Call"
emailInteractionType = "Email"
meetingInteractionType = "Meeting"

-- Other helpers -----------------------

intColumnToId :: String -> TTerm (r -> Maybe Int) -> TTerm (r -> String)
intColumnToId itype iid = lambda "r" $ Strings.concat [
  string $ decapitalize itype,
  "_",
  Optionals.maybe (string "") Literals.showInt32 (iid @@ var "r")]

interactionVertices :: String -> (String -> TTerm (r -> Maybe Int)) -> Pg.Vertex Term
interactionVertices itype column = vertex customerInteractionVertexLabel
  (intColumnToId itype (column "id"))
  [property "interactionType" $ constant $ just $ string itype,
   property "interactionDate" $ column "interaction_date",
   property "notes" $ column "notes",
   property "durationMinutes" $ column "duration_minutes",
   property "followUpRequired" $ column "follow_up_required",
   property "customerInitiated" $ column "customer_initiated"]

employeeEdges :: String -> (String -> TTerm (r -> Maybe Int)) -> Pg.Edge Term
employeeEdges itype column = simpleEdge employeeEdgeLabel
  (intColumnToId itype (column "id"))
  (intColumnToId employeeVertexLabel $ column "employee_id")
  []

customerEdges :: String -> (String -> TTerm (r -> Maybe Int)) -> Pg.Edge Term
customerEdges itype column = simpleEdge customerEdgeLabel
  (intColumnToId itype (column "id"))
  (intColumnToId customerVertexLabel $ column "customer_id")
  []

-- Mapping -----------------------------

employeeVertices :: Pg.Vertex Term
employeeVertices = vertex employeeVertexLabel (intColumnToId employeeVertexLabel $ employeesColumn "employee_id") [
  property "firstName" $ employeesColumn "first_name",
  property "lastName" $ employeesColumn "last_name",
  property "email" $ employeesColumn "email",
  property "hireDate" $ employeesColumn "hire_date"]

departmentVertices :: Pg.Vertex Term
departmentVertices = vertex departmentVertexLabel (intColumnToId departmentVertexLabel $ departmentsColumn "department_id") [
  property "name" $ departmentsColumn "department_name"]

customerVertices :: Pg.Vertex Term
customerVertices = vertex customerVertexLabel (intColumnToId customerVertexLabel $ customersColumn "customer_id") [
  property "companyName" $ customersColumn "company_name",
  property "contactName" $ customersColumn "contact_name",
  property "email" $ customersColumn "email",
  property "phone" $ customersColumn "phone"]

productVertices :: Pg.Vertex Term
productVertices = vertex productVertexLabel (intColumnToId productVertexLabel $ productsColumn "product_id") [
  property "name" $ productsColumn "product_name",
  property "price" $ productsColumn "price"]

saleVertices :: Pg.Vertex Term
saleVertices = vertex saleVertexLabel (intColumnToId saleVertexLabel $ salesColumn "sale_id") [
  property "saleDate" $ salesColumn "sale_date",
  property "totalAmount" $ salesColumn "total_amount"]

saleItemVertices :: Pg.Vertex Term
saleItemVertices = vertex saleItemVertexLabel (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id") [
  property "quantity" $ saleItemsColumn "quantity",
  property "itemPrice" $ saleItemsColumn "item_price"]

callInteractionVertices :: Pg.Vertex Term
callInteractionVertices = interactionVertices callInteractionType callsColumn

emailInteractionVertices :: Pg.Vertex Term
emailInteractionVertices = interactionVertices emailInteractionType emailsColumn

meetingInteractionVertices :: Pg.Vertex Term
meetingInteractionVertices = interactionVertices meetingInteractionType meetingsColumn

managesEdges :: Pg.Edge Term
managesEdges = simpleEdge managesEdgeLabel
  (intColumnToId employeeVertexLabel $ employeesColumn "manager_id")
  (intColumnToId employeeVertexLabel $ employeesColumn "employee_id")
  [property "sinceDate" $ employeesColumn "management_since_date"]

belongsToEdges :: Pg.Edge Term
belongsToEdges = simpleEdge belongsToEdgeLabel
  (intColumnToId employeeVertexLabel $ employeesColumn "employee_id")
  (intColumnToId departmentVertexLabel $ employeesColumn "department_id")
  [property "joinDate" $ employeesColumn "department_join_date",
   property "role" $ employeesColumn "department_role"]

parentDepartmentEdges :: Pg.Edge Term
parentDepartmentEdges = simpleEdge parentDepartmentEdgeLabel
  (intColumnToId departmentVertexLabel $ departmentsColumn "department_id")
  (intColumnToId departmentVertexLabel $ departmentsColumn "parent_department_id")
  [property "since" $ departmentsColumn "parent_department_since"]

soldEdges :: Pg.Edge Term
soldEdges = simpleEdge soldEdgeLabel
  (intColumnToId employeeVertexLabel $ salesColumn "employee_id")
  (intColumnToId saleVertexLabel $ salesColumn "sale_id")
  [property "commissionRate" $ salesColumn "commission_rate",
   property "salesChannel" $ salesColumn "sales_channel"]

purchasedEdges :: Pg.Edge Term
purchasedEdges = simpleEdge purchasedEdgeLabel
  (intColumnToId customerVertexLabel $ salesColumn "customer_id")
  (intColumnToId saleVertexLabel $ salesColumn "sale_id")
  [property "paymentMethod" $ salesColumn "payment_method",
   property "satisfactionRating" $ salesColumn "satisfaction_rating",
   property "isRepeatCustomer" $ salesColumn "is_repeat_customer"]

includesEdges :: Pg.Edge Term
includesEdges = simpleEdge includesEdgeLabel
  (intColumnToId saleVertexLabel $ saleItemsColumn "sale_id")
  (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id")
  [property "itemOrder" $ saleItemsColumn "item_order",
   property "discountApplied" $ saleItemsColumn "discount_applied"]

containsProductEdges :: Pg.Edge Term
containsProductEdges = simpleEdge containsProductEdgeLabel
  (intColumnToId saleItemVertexLabel $ saleItemsColumn "sale_item_id")
  (intColumnToId productVertexLabel $ saleItemsColumn "product_id")
  [property "warrantyPeriodYears" $ saleItemsColumn "warranty_period"]

callEmployeeEdges :: Pg.Edge Term
callEmployeeEdges = employeeEdges callInteractionType callsColumn

callCustomerEdges :: Pg.Edge Term
callCustomerEdges = customerEdges callInteractionType callsColumn

emailEmployeeEdges :: Pg.Edge Term
emailEmployeeEdges = employeeEdges emailInteractionType emailsColumn

emailCustomerEdges :: Pg.Edge Term
emailCustomerEdges = customerEdges emailInteractionType emailsColumn

meetingEmployeeEdges :: Pg.Edge Term
meetingEmployeeEdges = employeeEdges meetingInteractionType meetingsColumn

meetingCustomerEdges :: Pg.Edge Term
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
