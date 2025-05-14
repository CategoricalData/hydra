{-# LANGUAGE OverloadedStrings #-}

module Hydra.Demos.PropertyGraphGeneration.ExampleMapping where

import Hydra.Phantoms
import Hydra.Dsl.Base
import Hydra.Dsl.Pg.Mappings
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Strings as Strings


-- Helpers -----------------------------

callsColumn = columnValue "calls.csv"
customersColumn = columnValue "customers.csv"
departmentsColumn = columnValue "departments.csv"
emailsColumn = columnValue "emails.csv"
employeesColumn = columnValue "employees.csv"
meetingsColumn = columnValue "meetings.csv"
productsColumn = columnValue "products.csv"
saleItemsColumn = columnValue "sale_items.csv"
salesColumn = columnValue "sales.csv"

callInteractionType = "call"
emailInteractionType = "email"
meetingInteractionType = "meeting"

interactionId :: TTerm String -> TTerm (r -> Int) -> TTerm (r -> String)
interactionId itype iid = lambda "r" $ Strings.concat [itype, "_", Literals.showInt32 @@ (iid @@ var "r")]

interactionVertices itype column = vertex "CustomerInteraction"
  (interactionId itype (column "id"))
  [property "interactionType" itype,
   property "interactionDate" $ column "interaction_date",
   property "notes" $ column "notes",
   property "durationMinutes" $ column "duration_minutes",
   property "followUpRequired" $ column "follow_up_required",
   property "customerInitiated" $ column "customer_initiated"]

employeeEdges itype column = simpleEdge "employee"
  (interactionId itype (column "id"))
  (column "employee_id")
  []

customerEdges itype column = simpleEdge "customer"
  (interactionId itype (column "id"))
  (column "employee_id")
  []

-- Mapping -----------------------------

employeeVertices = vertex "Employee" (employeesColumn "employee_id") [
  property "firstName" $ employeesColumn "first_name",
  property "lastName" $ employeesColumn "last_name",
  property "email" $ employeesColumn "email",
  property "hireDate" $ employeesColumn "hire_date"]

departmentVertices = vertex "Department" (departmentsColumn "department_id") [
  property "name" $ departmentsColumn "department_name"]

customerVertices = vertex "Customer" (customersColumn "customer_id") [
  property "companyName" $ customersColumn "company_name",
  property "contactName" $ customersColumn "contact_name",
  property "email" $ customersColumn "email",
  property "phone" $ customersColumn "phone"]

productVertices = vertex "Product" (productsColumn "product_id") [
  property "name" $ productsColumn "product_name",
  property "price" $ productsColumn "price"]

saleVertices = vertex "Sale" (salesColumn "sale_id") [
  property "saleDate" $ salesColumn "sale_date",
  property "totalAmount" $ salesColumn "total_amount"]

saleItemVertices = vertex "SaleItem" (saleItemsColumn "sale_item_id") [
  property "quantity" $ saleItemsColumn "quantity",
  property "itemPrice" $ saleItemsColumn "item_price"]

callInteractionVertices = interactionVertices callInteractionType callsColumn
emailInteractionVertices = interactionVertices emailInteractionType emailsColumn
meetingInteractionVertices = interactionVertices meetingInteractionType meetingsColumn

managesEdges = simpleEdge "manages"
  (employeesColumn "manager_id")
  (employeesColumn "employee_id")
  [property "sinceDate" $ employeesColumn "management_since_date"]

belongsToEdges = simpleEdge "belongsTo"
  (employeesColumn "employee_id")
  (employeesColumn "department_id")
  [property "joinDate" $ employeesColumn "department_join_date",
   property "role" $ employeesColumn "department_role"]

parentDepartmentEdges = simpleEdge "parentDepartment"
  (departmentsColumn "department_id")
  (departmentsColumn "parent_department_id")
  [property "since" $ departmentsColumn "parent_department_since"]

soldEdges = simpleEdge "sold"
  (salesColumn "employee_id")
  (salesColumn "sale_id")
  [property "commissionRate" $ salesColumn "commission_rate",
   property "salesChannel" $ salesColumn "sales_channel"]

purchasedEdges = simpleEdge "purchased"
  (salesColumn "customer_id")
  (salesColumn "sale_id")
  [property "paymentMethod" $ salesColumn "payment_method",
   property "satisfactionRating" $ salesColumn "satisfaction_rating",
   property "isRepeatCustomer" $ salesColumn "is_repeat_customer"]

includesEdges = simpleEdge "includes"
  (saleItemsColumn "sale_id")
  (saleItemsColumn "sale_item_id")
  [property "itemOrder" $ saleItemsColumn "item_order",
   property "discountApplied" $ saleItemsColumn "discount_applied"]

containsProductEdges = simpleEdge "containsProduct"
  (saleItemsColumn "sale_item_id")
  (saleItemsColumn "product_id")
  [property "warrantyPeriodYears" $ saleItemsColumn "warranty_period"]

callEmployeeEdges = employeeEdges callInteractionType callsColumn
callCustomerEdges = customerEdges callInteractionType callsColumn
emailEmployeeEdges = employeeEdges emailInteractionType emailsColumn
emailCustomerEdges = customerEdges emailInteractionType emailsColumn
meetingEmployeeEdges = employeeEdges meetingInteractionType meetingsColumn
meetingCustomerEdges = customerEdges meetingInteractionType meetingsColumn

salesGraphMapping = graph
  [employeeVertices, departmentVertices, customerVertices, productVertices,
   saleVertices, saleItemVertices,
   callInteractionVertices, emailInteractionVertices, meetingInteractionVertices]
  [managesEdges, belongsToEdges, parentDepartmentEdges,
   soldEdges, purchasedEdges, includesEdges, containsProductEdges,
   callEmployeeEdges, callCustomerEdges,
   emailEmployeeEdges, emailCustomerEdges,
   meetingEmployeeEdges, meetingCustomerEdges]
