# Note: this is an automatically generated file. Do not edit.

r"""The Apache Atlas meta-model
Based on the the org.apache.atlas.model package in the master branch as of 2022-06-01
  https://github.com/apache/atlas/tree/master/intg/src/main/java/org/apache/atlas/model."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.xml.schema

@dataclass(frozen=True)
class AtlasAttributeDef:
    r"""class that captures details of a struct-attribute."""

    name: Maybe[str]
    type_name: Maybe[str]
    is_optional: bool
    cardinality: Maybe[AtlasAttributeDef_Cardinality]
    values_min_count: int
    values_max_count: int
    is_unique: bool
    is_indexable: bool
    include_in_notification: bool
    default_value: Maybe[str]
    description: Maybe[str]
    search_weight: int
    index_type: Maybe[AtlasAttributeDef_IndexType]
    constraints: frozenlist[AtlasConstraintDef]
    options: FrozenDict[str, str]
    display_name: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasAttributeDef")
    NAME = hydra.core.Name("name")
    TYPE_NAME = hydra.core.Name("typeName")
    IS_OPTIONAL = hydra.core.Name("isOptional")
    CARDINALITY = hydra.core.Name("cardinality")
    VALUES_MIN_COUNT = hydra.core.Name("valuesMinCount")
    VALUES_MAX_COUNT = hydra.core.Name("valuesMaxCount")
    IS_UNIQUE = hydra.core.Name("isUnique")
    IS_INDEXABLE = hydra.core.Name("isIndexable")
    INCLUDE_IN_NOTIFICATION = hydra.core.Name("includeInNotification")
    DEFAULT_VALUE = hydra.core.Name("defaultValue")
    DESCRIPTION = hydra.core.Name("description")
    SEARCH_WEIGHT = hydra.core.Name("searchWeight")
    INDEX_TYPE = hydra.core.Name("indexType")
    CONSTRAINTS = hydra.core.Name("constraints")
    OPTIONS = hydra.core.Name("options")
    DISPLAY_NAME = hydra.core.Name("displayName")

class AtlasAttributeDef_Cardinality(Enum):
    SINGLE = hydra.core.Name("single")

    LIST = hydra.core.Name("list")

    SET = hydra.core.Name("set")

AtlasAttributeDef_Cardinality.TYPE_ = hydra.core.Name("hydra.atlas.AtlasAttributeDef_Cardinality")

class AtlasAttributeDef_IndexType(Enum):
    DEFAULT = hydra.core.Name("default")

    STRING = hydra.core.Name("string")

AtlasAttributeDef_IndexType.TYPE_ = hydra.core.Name("hydra.atlas.AtlasAttributeDef_IndexType")

@dataclass(frozen=True)
class AtlasBaseTypeDef:
    r"""Base class that captures common-attributes for all Atlas types."""

    category: Maybe[TypeCategory]
    guid: Maybe[str]
    created_by: Maybe[str]
    updated_by: Maybe[str]
    create_time: Maybe[hydra.xml.schema.DateTime]
    update_time: Maybe[hydra.xml.schema.DateTime]
    version: Maybe[int]
    name: Maybe[str]
    description: Maybe[str]
    type_version: Maybe[str]
    service_type: Maybe[str]
    options: FrozenDict[str, str]

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasBaseTypeDef")
    CATEGORY = hydra.core.Name("category")
    GUID = hydra.core.Name("guid")
    CREATED_BY = hydra.core.Name("createdBy")
    UPDATED_BY = hydra.core.Name("updatedBy")
    CREATE_TIME = hydra.core.Name("createTime")
    UPDATE_TIME = hydra.core.Name("updateTime")
    VERSION = hydra.core.Name("version")
    NAME = hydra.core.Name("name")
    DESCRIPTION = hydra.core.Name("description")
    TYPE_VERSION = hydra.core.Name("typeVersion")
    SERVICE_TYPE = hydra.core.Name("serviceType")
    OPTIONS = hydra.core.Name("options")

@dataclass(frozen=True)
class AtlasConstraintDef:
    r"""class that captures details of a constraint."""

    type: Maybe[str]
    params: FrozenDict[str, str]

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasConstraintDef")
    TYPE = hydra.core.Name("type")
    PARAMS = hydra.core.Name("params")

@dataclass(frozen=True)
class AtlasEntityDef:
    r"""class that captures details of a entity-type."""

    as_atlas_struct: AtlasStructDef
    super_types: frozenset[str]
    sub_types: Annotated[frozenset[str], "the value of this field is derived from 'superTypes' specified in all AtlasEntityDef"]
    relationship_attribute_defs: Annotated[frozenlist[AtlasRelationshipAttributeDef], "the value of this field is derived from all the relationshipDefs this entityType is referenced in"]
    business_attribute_defs: Annotated[FrozenDict[str, frozenlist[AtlasAttributeDef]], "the value of this field is derived from all the businessMetadataDefs this entityType is referenced in"]

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasEntityDef")
    AS_ATLAS_STRUCT = hydra.core.Name("asAtlasStruct")
    SUPER_TYPES = hydra.core.Name("superTypes")
    SUB_TYPES = hydra.core.Name("subTypes")
    RELATIONSHIP_ATTRIBUTE_DEFS = hydra.core.Name("relationshipAttributeDefs")
    BUSINESS_ATTRIBUTE_DEFS = hydra.core.Name("businessAttributeDefs")

@dataclass(frozen=True)
class AtlasRelationshipAttributeDef:
    r"""class that captures details of a struct-attribute."""

    as_atlas_attribute: AtlasAttributeDef
    relationship_type_name: Maybe[str]
    is_legacy_attribute: bool

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasRelationshipAttributeDef")
    AS_ATLAS_ATTRIBUTE = hydra.core.Name("asAtlasAttribute")
    RELATIONSHIP_TYPE_NAME = hydra.core.Name("relationshipTypeName")
    IS_LEGACY_ATTRIBUTE = hydra.core.Name("isLegacyAttribute")

@dataclass(frozen=True)
class AtlasStructDef:
    r"""class that captures details of a struct-type."""

    as_atlas_base_type: AtlasBaseTypeDef
    attribute_defs: frozenlist[AtlasAttributeDef]

    TYPE_ = hydra.core.Name("hydra.atlas.AtlasStructDef")
    AS_ATLAS_BASE_TYPE = hydra.core.Name("asAtlasBaseType")
    ATTRIBUTE_DEFS = hydra.core.Name("attributeDefs")

class TypeCategory(Enum):
    PRIMITIVE = hydra.core.Name("primitive")

    OBJECT_ID_TYPE = hydra.core.Name("objectIdType")

    ENUM = hydra.core.Name("enum")

    STRUCT = hydra.core.Name("struct")

    CLASSIFICATION = hydra.core.Name("classification")

    ENTITY = hydra.core.Name("entity")

    ARRAY = hydra.core.Name("array")

    MAP = hydra.core.Name("map")

    RELATIONSHIP = hydra.core.Name("relationship")

    BUSINESS_METADATA = hydra.core.Name("businessMetadata")

TypeCategory.TYPE_ = hydra.core.Name("hydra.atlas.TypeCategory")
