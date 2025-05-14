module Hydra.Demos.PropertyGraphGeneration.ExampleDatabaseSchema where

import Hydra.Dsl.Ext.Tabular
import qualified Hydra.Dsl.Types as Types


employeesTableType :: TableType
employeesTableType = tableType "employees.csv" [
  columnType "employee_id" Types.int32,
  columnType "first_name" Types.string,
  columnType "last_name" Types.string,
  columnType "email" Types.string,
  columnType "hire_date" Types.string,
  columnType "manager_id" Types.int32,
  columnType "department_id" Types.int32,
  columnType "management_since_date" Types.string,
  columnType "department_join_date" Types.string,
  columnType "department_role" Types.string]

departmentsTableType :: TableType
departmentsTableType = tableType "departments.csv" [
  columnType "department_id" Types.int32,
  columnType "department_name" Types.string,
  columnType "parent_department_id" Types.int32,
  columnType "parent_department_since" Types.string]

customersTableType :: TableType
customersTableType = tableType "customers.csv" [
  columnType "customer_id" Types.int32,
  columnType "company_name" Types.string,
  columnType "contact_name" Types.string,
  columnType "email" Types.string,
  columnType "phone" Types.string]

productsTableType :: TableType
productsTableType = tableType "products.csv" [
  columnType "product_id" Types.int32,
  columnType "product_name" Types.string,
  columnType "price" Types.float64,
  columnType "category_name" Types.string,
  columnType "since_date" Types.string,
  columnType "is_primary" Types.boolean]

salesTableType :: TableType
salesTableType = tableType "sales.csv" [
  columnType "sale_id" Types.int32,
  columnType "employee_id" Types.int32,
  columnType "customer_id" Types.int32,
  columnType "sale_date" Types.string,
  columnType "total_amount" Types.float64,
  columnType "commission_rate" Types.float64,
  columnType "sales_channel" Types.string,
  columnType "payment_method" Types.string,
  columnType "satisfaction_rating" Types.int32,
  columnType "is_repeat_customer" Types.boolean]

saleItemsTableType :: TableType
saleItemsTableType = tableType "sale_items.csv" [
  columnType "sale_item_id" Types.int32,
  columnType "sale_id" Types.int32,
  columnType "product_id" Types.int32,
  columnType "quantity" Types.int32,
  columnType "item_price" Types.float64,
  columnType "item_order" Types.int32,
  columnType "discount_applied" Types.float64,
  columnType "warranty_period" Types.int32]

callsTableType :: TableType
callsTableType = tableType "calls.csv" [
  columnType "id" Types.int32,
  columnType "customer_id" Types.int32,
  columnType "employee_id" Types.int32,
  columnType "interaction_date" Types.string,
  columnType "notes" Types.string,
  columnType "duration_minutes" Types.int32,
  columnType "follow_up_required" Types.boolean,
  columnType "customer_initiated" Types.boolean]

emailsTableType :: TableType
emailsTableType = tableType "emails.csv" [
  columnType "id" Types.int32,
  columnType "customer_id" Types.int32,
  columnType "employee_id" Types.int32,
  columnType "interaction_date" Types.string,
  columnType "notes" Types.string,
  columnType "duration_minutes" Types.int32,
  columnType "follow_up_required" Types.boolean,
  columnType "customer_initiated" Types.boolean]

meetingsTableType :: TableType
meetingsTableType = tableType "meetings.csv" [
  columnType "id" Types.int32,
  columnType "customer_id" Types.int32,
  columnType "employee_id" Types.int32,
  columnType "interaction_date" Types.string,
  columnType "notes" Types.string,
  columnType "duration_minutes" Types.int32,
  columnType "follow_up_required" Types.boolean,
  columnType "customer_initiated" Types.boolean]

salesTableSchemas :: [TableType]
salesTableSchemas = [
  employeesTableType,
  departmentsTableType,
  customersTableType,
  productsTableType,
  salesTableType,
  saleItemsTableType,
  callsTableType,
  emailsTableType,
  meetingsTableType]
