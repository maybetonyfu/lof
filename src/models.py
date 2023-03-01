from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, Table, JSON
from sqlalchemy.orm import relationship

from src.database import Base

core_rule = Table(
    "core_rule",
    Base.metadata,
    Column("core_id", ForeignKey("core.id"), primary_key=True),
    Column("rule_id", ForeignKey("rule.id"), primary_key=True),
)

cause_rule = Table(
    "cause_rule",
    Base.metadata,
    Column("cause_id", ForeignKey("cause.id"), primary_key=True),
    Column("rule_id", ForeignKey("rule.id"), primary_key=True),
)


class Error:
    __tablename__ = "type_error"
    id = Column(Integer, primary_key=True, index=True)
    cores = relationship("Core", back_populates="from_error")
    causes = relationship("Cause", back_populates="from_error")


class Core:
    __tablename__ = "core"
    id = Column(Integer, primary_key=True, index=True)
    rules = relationship(secondary=core_rule)


class Cause:
    __tablename__ = "cause"
    id = Column(Integer, primary_key=True, index=True)
    rules = relationship(secondary=cause_rule)


class Rule:
    __table__ = "rule"
    id = Column(Integer, primary_key=True, index=True)
    decl = Column(String, index=True)
    from_line = Column(Integer)
    from_col = Column(Integer)
    to_line = Column(Integer)
    to_column = Column(Integer)
    file = Column(String)
    ambient = Column(Boolean)
    type = Column(String)
    src_text = Column(String)


class Typing:
    __table__ = "typing"
    id = Column(Integer, primary_key=True, index=True)
    expression = Column(String, index=True)
    type = Column(JSON)
    signature = Column(String, index=True)
