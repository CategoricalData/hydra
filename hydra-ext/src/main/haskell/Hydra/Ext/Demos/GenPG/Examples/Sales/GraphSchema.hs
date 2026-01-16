module Hydra.Ext.Demos.GenPG.Examples.Sales.GraphSchema where

import Hydra.Ext.Dsl.Pg.Schemas (propertyType, required, schema, simpleEdgeType, vertexType)
import Hydra.Dsl.Types (binary, boolean, float32, float64, int32, int64, string)

dateType = string
decimalType = float64

-- Vertex labels -----------------------

employeeVertexLabel = "Employee"
departmentVertexLabel = "Department"
customerVertexLabel = "Customer"
productVertexLabel = "Product"
saleVertexLabel = "Sale"
saleItemVertexLabel = "SaleItem"
customerInteractionVertexLabel = "CustomerInteraction"

-- Edge labels -------------------------

managesEdgeLabel = "manages"
belongsToEdgeLabel = "belongsTo"
parentDepartmentEdgeLabel = "parentDepartment"
soldEdgeLabel = "sold"
purchasedEdgeLabel = "purchased"
includesEdgeLabel = "includes"
containsProductEdgeLabel = "containsProduct"
employeeEdgeLabel = "employee"
customerEdgeLabel = "customer"

-- Interaction types -------------------

callInteractionType = "Call"
emailInteractionType = "Email"
meetingInteractionType = "Meeting"

-- Graph Schema ------------------------

salesGraphSchema = schema vertexTypes edgeTypes
  where
    vertexTypes = [
      vertexType employeeVertexLabel int32 [
        required $ propertyType "firstName" string,
        required $ propertyType "lastName" string,
        required $ propertyType "email" string,
        propertyType "hireDate" dateType],

      vertexType departmentVertexLabel int32 [
        required $ propertyType "name" string],

      vertexType customerVertexLabel int32 [
        required $ propertyType "companyName" string,
        propertyType "contactName" string,
        propertyType "email" string,
        propertyType "phone" string],

      vertexType productVertexLabel int32 [
        required $ propertyType "name" string,
        propertyType "price" decimalType],

      vertexType saleVertexLabel int32 [
        required $ propertyType "saleDate" dateType,
        propertyType "totalAmount" decimalType],

      vertexType saleItemVertexLabel int32 [
        propertyType "quantity" int32,
        propertyType "itemPrice" decimalType],

      vertexType customerInteractionVertexLabel string [
        required $ propertyType "interactionDate" dateType,
        required $ propertyType "interactionType" string,
        propertyType "notes" string,
        propertyType "durationMinutes" int32,
        propertyType "followUpRequired" boolean,
        propertyType "customerInitiated" boolean]]

    edgeTypes = [
      simpleEdgeType managesEdgeLabel employeeVertexLabel employeeVertexLabel [
        propertyType "sinceDate" dateType],

      simpleEdgeType belongsToEdgeLabel employeeVertexLabel departmentVertexLabel [
        required $ propertyType "joinDate" dateType,
        propertyType "role" string],

      simpleEdgeType parentDepartmentEdgeLabel departmentVertexLabel departmentVertexLabel [
        propertyType "since" dateType],

      simpleEdgeType "hasCategory" productVertexLabel "Category" [
        propertyType "sinceDate" dateType,
        required $ propertyType "isPrimary" boolean],

      simpleEdgeType soldEdgeLabel employeeVertexLabel saleVertexLabel [
        required $ propertyType "commissionRate" float64,
        propertyType "salesChannel" string],

      simpleEdgeType purchasedEdgeLabel customerVertexLabel saleVertexLabel [
        required $ propertyType "paymentMethod" string,
        propertyType "satisfactionRating" int32,
        propertyType "isRepeatCustomer" boolean],

      simpleEdgeType includesEdgeLabel saleVertexLabel saleItemVertexLabel [
        required $ propertyType "itemOrder" int32,
        propertyType "discountApplied" float64],

      simpleEdgeType containsProductEdgeLabel saleItemVertexLabel productVertexLabel [
        propertyType "warrantyPeriodYears" int32],

      simpleEdgeType employeeEdgeLabel customerInteractionVertexLabel employeeVertexLabel [],

      simpleEdgeType customerEdgeLabel customerInteractionVertexLabel customerVertexLabel []]
